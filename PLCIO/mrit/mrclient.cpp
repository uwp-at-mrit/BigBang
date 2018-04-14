#include <ppltasks.h>

#include "mrit/mrclient.hpp"
#include "mrit/magic.hpp"
#include "mrit/message.hpp"

#include "modbus/exception.hpp"
#include "modbus/dataunit.hpp"
#include "shared/netexn.hpp"

#include "syslog.hpp"

using namespace WarGrey::SCADA;

using namespace Concurrency;
using namespace Windows::Foundation;
using namespace Windows::Networking;
using namespace Windows::Networking::Sockets;
using namespace Windows::Storage::Streams;

static void modbus_apply_positive_confirmation(IMRConfirmation* cf, Syslog* logger, uint8 function_code
	, uint16 data_block, uint16 start_address, uint16 end_address, uint8* data_pool, uint16 data_size) {
}

/*************************************************************************************************/
IMRClient::IMRClient(Syslog* sl, Platform::String^ h, uint16 p, IMRConfirmation* cf) {
	this->logger = ((sl == nullptr) ? make_silent_logger("Silent MRIT Client") : sl);
	this->logger->reference();

	this->device = ref new HostName(h);
    this->service = p.ToString();

	this->confirmation = cf;

    this->connect();
};

IMRClient::~IMRClient() {
	delete this->socket; // stop the confirmation loop before release transactions.

	this->logger->destroy();
}

Platform::String^ IMRClient::device_hostname() {
	return this->device->RawName;
}

Syslog* IMRClient::get_logger() {
	return this->logger;
}

void IMRClient::send_scheduled_request(long long count, long long interval, long long uptime) {
	if (this->confirmation != nullptr) {
		this->confirmation->on_scheduled_request(this, count, interval, uptime);
	}
}

void IMRClient::connect() {
	if (this->mrout != nullptr) {
		delete this->socket;
		delete this->mrin;
		delete this->mrout;

		this->mrout = nullptr;
	}

	this->socket = ref new StreamSocket();
	this->socket->Control->KeepAlive = false;

	this->logger->log_message(Log::Debug, L">> connecting to %s:%s", this->device->RawName->Data(), this->service->Data());

    create_task(this->socket->ConnectAsync(this->device, this->service)).then([this](task<void> handshaking) {
        try {
            handshaking.get();

            this->mrin  = ref new DataReader(this->socket->InputStream);
            this->mrout = ref new DataWriter(this->socket->OutputStream);

            mrin->UnicodeEncoding = UnicodeEncoding::Utf8;
            mrin->ByteOrder = ByteOrder::BigEndian;
            mrout->UnicodeEncoding = UnicodeEncoding::Utf8;
            mrout->ByteOrder = ByteOrder::BigEndian;

            this->logger->log_message(Log::Info, L">> connected to %s:%s", this->device->RawName->Data(), this->service->Data());

			this->wait_process_confirm_loop();
        } catch (Platform::Exception^ e) {
			this->logger->log_message(Log::Warning, socket_strerror(e));
			this->connect();
        }
    });
}

void IMRClient::request(uint8 fcode, uint16 data_block, uint16 addr0, uint16 addrn, uint8* data, uint16 size) {
	mr_write_header(this->mrout, fcode, data_block, addr0, addrn);
	mr_write_tail(this->mrout, data, size);

	create_task(this->mrout->StoreAsync()).then([=](task<unsigned int> sending) {
		try {
			unsigned int sent = sending.get();

			this->logger->log_message(Log::Debug,
				L"<sent %u-byte-request for command '%c' on data block %hu[%hu, %hu] to %s:%s>",
				sent, fcode, data_block, addr0, addrn,
				this->device->RawName->Data(), this->service->Data());
		} catch (task_canceled&) {
		} catch (Platform::Exception^ e) {
			this->logger->log_message(Log::Warning, e->Message);
			this->connect();
		}});
}

void IMRClient::wait_process_confirm_loop() {
	create_task(this->mrin->LoadAsync(MR_PROTOCOL_HEADER_LENGTH)).then([=](unsigned int size) {
		uint8 head, fcode;
		uint16 data_block, start_address, end_address;
		uint16 data_size;

		if (size < MR_PROTOCOL_HEADER_LENGTH) {
			if (size == 0) {
				modbus_protocol_fatal(this->logger,
					L"Server %s:%s has lost",
					this->device->RawName->Data(), this->service->Data());
			} else {
				modbus_protocol_fatal(this->logger,
					L"message header comes from server %s:%s is too short(%u < %hu)",
					this->device->RawName->Data(), this->service->Data(),
					size, MR_PROTOCOL_HEADER_LENGTH);
			}
		}

		uint16 tail_size = mr_read_header(mrin, &head, &fcode, &data_block, &start_address, &end_address, &data_size);

		return create_task(this->mrin->LoadAsync(tail_size)).then([=](unsigned int size) {
			uint16 unused_checksum, end_of_message;

			if (size < tail_size) {
				modbus_protocol_fatal(this->logger,
					L"message comes from server %s:%s has been truncated(%u < %hu)",
					this->device->RawName->Data(), this->service->Data(),
					size, tail_size);
			}

			if (head != MR_PROTOCOL_HEAD) {
				modbus_discard_current_adu(this->logger,
					L"<discarded non-mrit-tcp message(%hhu, %hhu, %hu, %hu, %hu, %hu) comes from %s:%s>",
					head, fcode, data_block, start_address, end_address, data_size,
					this->device->RawName->Data(), this->service->Data());
			}

			uint8* data_pool = new uint8[data_size];
			mr_read_tail(this->mrin, data_size, data_pool, &unused_checksum, &end_of_message);

			if (end_of_message != MR_PROTOCOL_END) {
				delete[] data_pool;

				modbus_protocol_fatal(this->logger,
					L"message comes from server %s:%s has an malformed end(expected %hu, received %hu)",
					this->device->RawName->Data(), this->service->Data(),
					MR_PROTOCOL_END, end_of_message);
			} else {
				this->logger->log_message(Log::Debug,
					L"<received confirmation(%hhu, %hhu, %hu, %hu, %hu) for function 0x%02X comes from %s:%s>",
					head, fcode, data_block, start_address, end_address,
					this->device->RawName->Data(), this->service->Data());

				if (this->confirmation != nullptr) {
					modbus_apply_positive_confirmation(this->confirmation, this->logger,
						fcode, data_block, start_address, end_address, data_pool, data_size);
				}

				delete[] data_pool;
			}
		});
	}).then([=](task<void> confirm) {
		try {
			confirm.get();

			unsigned int dirty = discard_dirty_bytes(this->mrin);

			if (dirty > 0) {
				this->logger->log_message(Log::Debug,
					L"<discarded last %u bytes of the confirmation comes from %s:%s>",
					dirty, this->device->RawName->Data(), this->service->Data());
			}

			this->wait_process_confirm_loop();
		} catch (task_discarded&) {
			discard_dirty_bytes(this->mrin);
			this->wait_process_confirm_loop();
		} catch (task_canceled&) {
			this->connect();
		} catch (Platform::Exception^ e) {
			this->logger->log_message(Log::Warning, e->Message);
			this->connect();
		}
	});
}

bool IMRClient::connected() {
	return (this->mrout != nullptr);
}

void IMRClient::read_signal(uint16 data_block, uint16 addr0, uint16 addrn) {
	this->request(MR_READ_SIGNAL, data_block, addr0, addrn, nullptr, 0);
}

/*************************************************************************************************/
void MRClient::read_realtime_data() {
	uint8 data_block;
	uint16 addr0, addrn;
	//this->read_signal();
}

void MRClient::read_analog_input_data(bool raw) {

}

void MRClient::read_analog_output_data(bool raw) {

}

void MRClient::read_digital_input_data(bool raw) {

}

void MRClient::read_digital_output_data(bool raw) {

}

void MRClient::write_analog_quantity(uint16 data_block, uint16 addr0, uint16 addrn) {

}

void MRClient::write_digital_quantity(uint16 data_block, uint16 addr0, uint16 addrn) {

}
