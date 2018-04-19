#include <ppltasks.h>

#include "mrit/magic.hpp"
#include "mrit/message.hpp"
#include "mrit/mrmaster.hpp"

#include "shared/netexn.hpp"

#include "modbus/exception.hpp"
#include "modbus/dataunit.hpp"

#include "syslog.hpp"

using namespace WarGrey::SCADA;

using namespace Concurrency;
using namespace Windows::Foundation;
using namespace Windows::Networking;
using namespace Windows::Networking::Sockets;
using namespace Windows::Storage::Streams;

static void modbus_apply_positive_confirmation(std::list<IMRConfirmation*> cfs, Syslog* logger, uint8 function_code
	, uint16 data_block, uint16 start_address, uint16 end_address, uint8* data_pool, uint16 data_size) {
}

/*************************************************************************************************/
IMRMaster::IMRMaster(Syslog* sl, Platform::String^ h, uint16 p, IMRConfirmation* cf) {
	this->logger = ((sl == nullptr) ? make_silent_logger("Silent MRIT Client") : sl);
	this->logger->reference();

	this->append_confirmation_receiver(cf);
    this->service = p.ToString();

	if (h == nullptr) {
		this->listener = new StreamListener();
	} else {
		this->device = ref new HostName(h);
	}

    this->shake_hands();
};

IMRMaster::~IMRMaster() {
	if (this->socket != nullptr) {
		delete this->socket; // stop the confirmation loop before release transactions.
	}

	if (this->listener != nullptr) {
		delete this->listener;
	}

	this->logger->destroy();
}

Platform::String^ IMRMaster::device_hostname() {
	Platform::String^ hostname = nullptr;

	if (this->device != nullptr) {
		hostname = this->device->RawName;
	} else if (this->socket != nullptr) {
		hostname = this->socket->Information->RemoteAddress->DisplayName;
	}

	return hostname;
}

Syslog* IMRMaster::get_logger() {
	return this->logger;
}

void IMRMaster::append_confirmation_receiver(IMRConfirmation* confirmation) {
	if (confirmation != nullptr) {
		this->confirmations.push_back(confirmation);
	}
}

void IMRMaster::shake_hands() {
	if (this->listener != nullptr) {
		this->listen();
	} else {
		this->connect();
	}
}

void IMRMaster::connect() {
	this->clear();
	this->socket = ref new StreamSocket();
	this->socket->Control->KeepAlive = false;

	this->logger->log_message(Log::Debug,
		L">> connecting to device[%s:%s]",
		this->device->RawName->Data(), this->service->Data());

    create_task(this->socket->ConnectAsync(this->device, this->service)).then([this](task<void> handshaking) {
        try {
            handshaking.get();

            this->mrin  = make_socket_reader(this->socket);
            this->mrout = make_socket_writer(this->socket);

            this->logger->log_message(Log::Info,
				L">> connected to device[%s:%s]",
				this->device->RawName->Data(), this->service->Data());

			this->wait_process_confirm_loop();
        } catch (Platform::Exception^ e) {
			this->logger->log_message(Log::Warning, socket_strerror(e));
			this->shake_hands();
        }
    });
}

void IMRMaster::listen() {
	if (this->listener != nullptr) {
		try {
			this->listener->listen(this, this->service);
			this->logger->log_message(Log::Info, L"## listening on %s:%s", L"0.0.0.0", this->service->Data());
		} catch (Platform::Exception^ e) {
			this->logger->log_message(Log::Warning, e->Message);
		}
	}
}

void IMRMaster::on_socket(StreamSocket^ plc) {
	this->clear();

	this->socket = plc;
	this->mrin  = make_socket_reader(plc);
	this->mrout = make_socket_writer(plc);

	this->logger->log_message(Log::Info, L"# device[%s] is accepted", socket_description(plc)->Data());

	this->wait_process_confirm_loop();
}

void IMRMaster::request(uint8 fcode, uint16 data_block, uint16 addr0, uint16 addrn, uint8* data, uint16 size) {
	if (this->mrout != nullptr) {
		mr_write_header(this->mrout, fcode, data_block, addr0, addrn);
		mr_write_tail(this->mrout, data, size);

		create_task(this->mrout->StoreAsync()).then([=](task<unsigned int> sending) {
			try {
				unsigned int sent = sending.get();

				this->logger->log_message(Log::Debug,
					L"<sent %u-byte-request for command '%c' on data block %hu[%hu, %hu] to device[%s:%s]>",
					sent, fcode, data_block, addr0, addrn,
					this->device->RawName->Data(), this->service->Data());
			} catch (task_canceled&) {
			} catch (Platform::Exception^ e) {
				this->logger->log_message(Log::Warning, e->Message);
				this->shake_hands();
			}
		});
	}
}

void IMRMaster::wait_process_confirm_loop() {
	create_task(this->mrin->LoadAsync(MR_PROTOCOL_HEADER_LENGTH)).then([=](unsigned int size) {
		uint8 leading_head, fcode;
		uint16 data_block, start_address, end_address;
		uint16 data_size;

		if (size < MR_PROTOCOL_HEADER_LENGTH) {
			if (size == 0) {
				modbus_protocol_fatal(this->logger,
					L"device[%s:%s] has lost",
					this->device->RawName->Data(), this->service->Data());
			} else {
				modbus_protocol_fatal(this->logger,
					L"message header comes from device[%s:%s] is too short(%u < %hu)",
					this->device->RawName->Data(), this->service->Data(),
					size, MR_PROTOCOL_HEADER_LENGTH);
			}
		}

		uint16 tail_size = mr_read_header(mrin, &leading_head, &fcode, &data_block, &start_address, &end_address, &data_size);

		if (leading_head != MR_PROTOCOL_HEAD) {
			modbus_discard_current_adu(this->logger,
				L"<discarded non-mrit-tcp message(%hhu, %hhu, %hu, %hu, %hu, %hu) comes from device[%s:%s]>",
				leading_head, fcode, data_block, start_address, end_address, data_size,
				this->device->RawName->Data(), this->service->Data());
		}

		return create_task(this->mrin->LoadAsync(tail_size)).then([=](unsigned int size) {
			uint16 unused_checksum, end_of_message;

			if (size < tail_size) {
				modbus_protocol_fatal(this->logger,
					L"message comes from server device[%s:%s] has been truncated(%u < %hu)",
					this->device->RawName->Data(), this->service->Data(),
					size, tail_size);
			}

			uint8* data_pool = new uint8[data_size];
			mr_read_tail(this->mrin, data_size, data_pool, &unused_checksum, &end_of_message);

			if (end_of_message != MR_PROTOCOL_END) {
				delete[] data_pool;

				modbus_protocol_fatal(this->logger,
					L"message comes from devce[%s:%s] has an malformed end(expected %hu, received %hu)",
					this->device->RawName->Data(), this->service->Data(),
					MR_PROTOCOL_END, end_of_message);
			} else {
				this->logger->log_message(Log::Debug,
					L"<received confirmation(%hhu, %hhu, %hu, %hu, %hu) for function 0x%02X comes from device[%s:%s]>",
					leading_head, fcode, data_block, start_address, end_address,
					this->device->RawName->Data(), this->service->Data());

				if (!this->confirmations.empty()) {
					modbus_apply_positive_confirmation(this->confirmations, this->logger,
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
					L"<discarded last %u bytes of the confirmation comes from device[%s:%s]>",
					dirty, this->device->RawName->Data(), this->service->Data());
			}

			this->wait_process_confirm_loop();
		} catch (task_discarded&) {
			discard_dirty_bytes(this->mrin);
			this->wait_process_confirm_loop();
		} catch (task_terminated&) {
			this->shake_hands();
		} catch (task_canceled&) {
			this->shake_hands();
		} catch (Platform::Exception^ e) {
			this->logger->log_message(Log::Warning, e->Message);
			this->shake_hands();
		}
	});
}

bool IMRMaster::connected() {
	return (this->mrout != nullptr);
}

void IMRMaster::clear() {
	if (this->mrout != nullptr) {
		delete this->socket;
		delete this->mrin;
		delete this->mrout;

		this->mrout = nullptr;
	}
}

/*************************************************************************************************/
void MRMaster::read_all_signal(uint16 data_block, uint16 addr0, uint16 addrn) {
	this->request(MR_READ_SIGNAL, data_block, addr0, addrn, nullptr, 0);
}

void MRMaster::write_analog_quantity(uint16 data_block, uint16 addr0, uint16 addrn) {

}

void MRMaster::write_digital_quantity(uint16 data_block, uint16 addr0, uint16 addrn) {

}
