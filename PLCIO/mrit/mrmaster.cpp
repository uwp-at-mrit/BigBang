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

static void dispatch_confirmation(Syslog* logger, std::list<IMRConfirmation*> cfs, uint16 db, float* data, uint16 count) {
	switch (db) {
	case MRDB_REALTIME: for (auto cf : cfs) { cf->on_realtime_data(data, count, logger); } break;
	case MRDB_FORAT: for (auto cf : cfs) { cf->on_forat_data(data, count, logger); } break;
	case MRDB_ANALOG_INPUT: for (auto cf : cfs) { cf->on_analog_input_data(data, count, logger); } break;
	case MRDB_ANALOG_INPUT_RAW: for (auto cf : cfs) { cf->on_raw_analog_input_data(data, count, logger); } break;
	case MRDB_ANALOG_OUTPUT: for (auto cf : cfs) { cf->on_analog_output_data(data, count, logger); } break;
	case MRDB_ANALOG_OUTPUT_RAW: for (auto cf : cfs) { cf->on_raw_analog_output_data(data, count, logger); } break;
	}
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

Platform::String^ IMRMaster::device_description() {
	Platform::String^ desc = nullptr;

	if (this->device != nullptr) {
		desc = this->device->RawName + ":" + this->service;
	} else if (this->socket != nullptr) {
		desc = socket_remote_description(this->socket);
	}

	return desc;
}

Syslog* IMRMaster::get_logger() {
	return this->logger;
}

void IMRMaster::set_message_alignment_size(uint16 size) {
	this->alignment_size = size;
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
	// TODO: should the `this->listener == nullptr` be checked here? 
	this->clear();
	this->socket = ref new StreamSocket();
	this->socket->Control->KeepAlive = false;

	this->logger->log_message(Log::Debug, L">> connecting to device[%s]", this->device_description()->Data());

    create_task(this->socket->ConnectAsync(this->device, this->service)).then([this](task<void> handshaking) {
        try {
            handshaking.get();

            this->mrin  = make_socket_reader(this->socket);
            this->mrout = make_socket_writer(this->socket);

            this->logger->log_message(Log::Debug, L">> connected to device[%s]", this->device_description()->Data());

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
			this->clear();
			this->listener->listen(this, this->service);
			this->logger->log_message(Log::Debug, L"## listening on 0.0.0.0:%s", this->service->Data());
		} catch (Platform::Exception^ e) {
			this->logger->log_message(Log::Warning, e->Message);
		}
	}
}

void IMRMaster::on_socket(StreamSocket^ plc) {
	this->socket = plc;
	this->mrin  = make_socket_reader(plc);
	this->mrout = make_socket_writer(plc);

	this->logger->log_message(Log::Debug, L"# device[%s] is accepted", this->device_description()->Data());

	this->wait_process_confirm_loop();
}

void IMRMaster::request(uint8 fcode, uint16 data_block, uint16 addr0, uint16 addrn, uint8* data, uint16 size) {
	if (this->mrout != nullptr) {
		uint16 foldsum;
		
		foldsum = mr_write_header(this->mrout, fcode, data_block, addr0, addrn);
		foldsum = mr_write_tail(this->mrout, data, size, foldsum, this->alignment_size);

		create_task(this->mrout->StoreAsync()).then([=](task<unsigned int> sending) {
			try {
				unsigned int sent = sending.get();

				this->logger->log_message(Log::Info,
					L"<sent %u-byte-request for command '%c' on data block %hu[%hu, %hu] to device[%s]>",
					sent, fcode, data_block, addr0, addrn, this->device_description()->Data());
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
				modbus_protocol_fatal(this->logger, L"device[%s] has lost", this->device_description()->Data());
			} else {
				modbus_protocol_fatal(this->logger,
					L"message header comes from device[%s] is too short(%u < %hu)",
					this->device_description()->Data(), size, MR_PROTOCOL_HEADER_LENGTH);
			}
		}

		uint16 tail_size = mr_read_header(mrin, &leading_head, &fcode, &data_block, &start_address, &end_address, &data_size);

		if (leading_head != MR_PROTOCOL_HEAD) {
			modbus_discard_current_adu(this->logger,
				L"<discarded non-mrit-tcp message(%hhu, %hhu, %hu, %hu, %hu, %hu) comes from device[%s]>",
				leading_head, fcode, data_block, start_address, end_address, data_size, this->device_description()->Data());
		}

		return create_task(this->mrin->LoadAsync(tail_size)).then([=](unsigned int size) {
			uint16 unused_checksum, end_of_message;

			if (size < tail_size) {
				modbus_protocol_fatal(this->logger,
					L"message comes from server device[%s] has been truncated(%u < %hu)",
					this->device_description()->Data(), size, tail_size);
			}

			uint8* data_pool = new uint8[data_size];
			mr_read_tail(this->mrin, data_size, data_pool, &unused_checksum, &end_of_message);

			if (end_of_message != MR_PROTOCOL_END) {
				delete[] data_pool;

				modbus_protocol_fatal(this->logger,
					L"message comes from devce[%s] has an malformed end(expected %hu, received %hu)",
					this->device_description()->Data(), MR_PROTOCOL_END, end_of_message);
			} else {
				this->logger->log_message(Log::Info,
					L"<received confirmation(%hu, %hu, %hu) comes for command '%c' from device[%s]>",
					data_block, start_address, end_address, fcode, this->device_description()->Data());

				if (!this->confirmations.empty()) {
					this->apply_confirmation(fcode, data_block, start_address, end_address, data_pool, data_size);
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
					L"<discarded last %u bytes of the confirmation comes from device[%s]>",
					dirty, this->device_description()->Data());
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

void IMRMaster::apply_confirmation(uint8 fcode, uint16 db, uint16 addr0, uint16 addrn, uint8* data, uint16 size) {
	if (fcode == MR_READ_SIGNAL) {
		if (db == MRDB_FOR_ALL) {
			this->on_all_signals(addr0, addrn, data, size);
		}
	}
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
void MRMaster::read_all_signal(uint16 data_block, uint16 addr0, uint16 addrn, float tidemark) {
	uint8 flbytes[4];

	set_bigendian_float(flbytes, 0, tidemark);

	this->request(MR_READ_SIGNAL, data_block, addr0, addrn, flbytes, 4);
}

void MRMaster::write_analog_quantity(uint16 data_block, uint16 addr0, uint16 addrn) {

}

void MRMaster::write_digital_quantity(uint16 data_block, uint16 addr0, uint16 addrn) {

}

void MRMaster::on_all_signals(uint16 addr0, uint16 addrn, uint8* data, uint16 size) {
	uint16 count, subaddr0, subaddrn;
	uint16 adbs[] = { 3, 203, 5, 204, 20, 2 };
	uint16 ddbs[] = { 4, 6, 205 };

	for (size_t i = 0; i < sizeof(adbs) / sizeof(uint16); i++) {
		if (this->fill_signal_preferences(adbs[i], &count, &subaddr0, &subaddrn)) {
			size_t flsize = sizeof(float);

			if (adbs[i] == MRDB_FORAT) {
				// this is a special case, some digital data is stored in the first bytes. 
				subaddr0 += 2;
				count -= 2;
			}

			if (((subaddrn - subaddr0 + 1) != (count * flsize))) {
				this->logger->log_message(Log::Warning,
					L"the address range [%hu, %hu] of DB%hu is misconfigured or mistyped(count: %hu != %hu)",
					adbs[i], subaddr0, subaddrn, count, (subaddrn - subaddr0 + 1) / flsize);
			}

			if ((subaddr0 + count * flsize) < size) {
				float* decoded_data = new float[count];

				read_bigendian_floats(data, subaddr0, count, decoded_data);
				dispatch_confirmation(this->logger, this->confirmations, adbs[i], decoded_data, count);
				
				delete[] decoded_data;
			} else {
				this->logger->log_message(Log::Error,
					L"the end address of DB%hu is out of range (%hu !<- [%hu, %hu])",
					subaddrn, addr0, addrn);
			}
		} else {
			this->logger->log_message(Log::Warning, L"missing configuration for data block %hu", adbs[i]);
		}
	}
}

