#include <ppltasks.h>

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

/*************************************************************************************************/
IMRMaster::IMRMaster(Syslog* sl, PLCMasterMode mode, Platform::String^ h, uint16 p, IMRConfirmation* cf) {
	this->logger = ((sl == nullptr) ? make_silent_logger("Silent MRIT Client") : sl);
	this->logger->reference();

	this->append_confirmation_receiver(cf);
    this->service = p.ToString();
	this->mode = mode;

	if (h == nullptr) {
		this->listener = new StreamListener();
	} else {
		this->device = ref new HostName(h);
	}

	this->pool_size = 0;

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

	if (this->mode == PLCMasterMode::Debug) {
		hostname += "[Debug]";
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

void IMRMaster::set_message_preference(WarGrey::SCADA::MrMessageConfiguration& config) {
	this->preference = config;
}

void IMRMaster::append_confirmation_receiver(IMRConfirmation* confirmation) {
	if (confirmation != nullptr) {
		this->confirmations.push_back(confirmation);
	}
}

void IMRMaster::append_plc_status_listener(WarGrey::SCADA::IPLCStatusListener* listener) {
	if (listener != nullptr) {
		this->listeners.push_back(listener);
		listener->on_plc_connectivity_changed(this, this->connected());
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
			this->delay_balance = 0;

            this->logger->log_message(Log::Debug, L">> connected to device[%s]", this->device_description()->Data());

			this->notify_connectivity_changed();
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
	
	this->notify_connectivity_changed();
	this->wait_process_confirm_loop();
}

void IMRMaster::request(size_t fcode, size_t datablock, size_t addr0, size_t addrn, uint8* data, size_t size) {
	if (this->mrout != nullptr) {
		bool reading = (fcode == this->preference.read_signal_fcode());
		bool reading_safe = true;

		if (reading) {
			if (this->delay_balance > 0) {
				reading_safe = false;

				syslog(Log::Notice,
					L"requesting for command '%c' on data block %u[%u, %u] to device[%s] is cancelled because of network delay",
					fcode, datablock, addr0, addrn, this->device_description()->Data());
			}
		}

		if (!reading || reading_safe) {
			this->preference.write_header(this->mrout, fcode, datablock, addr0, addrn);
			this->preference.write_aligned_tail(this->mrout, data, size);

			create_task(this->mrout->StoreAsync()).then([=](task<unsigned int> sending) {
				try {
					unsigned int sent = sending.get();

					if (reading) {
						this->delay_balance += 1;
					}

					this->logger->log_message(Log::Debug,
						L"<sent %u-byte-request for command '%c' on data block %u[%u, %u] to device[%s]>",
						sent, fcode, datablock, addr0, addrn, this->device_description()->Data());
				} catch (task_canceled&) {
				} catch (Platform::Exception^ e) {
					this->logger->log_message(Log::Warning, e->Message);
					this->shake_hands();
				}
			});
		}
	}
}

void IMRMaster::wait_process_confirm_loop() {
	unsigned int predata_size = (unsigned int)this->preference.predata_size();
	unsigned int postdata_size = (unsigned int)this->preference.postdata_size();

	create_task(this->mrin->LoadAsync(predata_size)).then([=](unsigned int size) {
		size_t leader, fcode, datablock, addr0, addrn, datasize, tailsize;
		
		if (size < predata_size) {
			if (size == 0) {
				modbus_protocol_fatal(this->logger, L"device[%s] has lost", this->device_description()->Data());
			} else {
				modbus_protocol_fatal(this->logger,
					L"message header comes from device[%s] is too short(%u < %u)",
					this->device_description()->Data(), size, predata_size);
			}
		}

		tailsize = this->preference.read_header(mrin, &leader, &fcode, &datablock, &addr0, &addrn, &datasize);

		if (!this->preference.header_match(leader)) {
			modbus_discard_current_adu(this->logger,
				L"<discarded non-mrit-tcp message(%u, %u, %u, %u, %u, %u) comes from device[%s]>",
				leader, fcode, datablock, addr0, addrn, datasize,
				this->device_description()->Data());
		}

		return create_task(this->mrin->LoadAsync((unsigned int)tailsize)).then([=](unsigned int size) {
			size_t unused_checksum, end_of_message, expected_tail;

			if (size < tailsize) {
				modbus_protocol_fatal(this->logger,
					L"message comes from device[%s] has been truncated(%u < %u)",
					this->device_description()->Data(), size, tailsize);
			}

			if (this->pool_size < datasize) {
				this->pool_size = (unsigned int)datasize;
				this->data_pool = ref new Platform::Array<uint8>(this->pool_size);				
			}

			this->preference.read_body_tail(this->mrin, this->data_pool, &unused_checksum, &end_of_message);

			if (!this->preference.tail_match(end_of_message, &expected_tail)) {
				modbus_protocol_fatal(this->logger,
					L"message comes from devce[%s] has an malformed end(expected 0X%04X, received 0X%04X)",
					this->device_description()->Data(), expected_tail, end_of_message);
			} else {
				this->logger->log_message(Log::Debug,
					L"<received confirmation(%u, %u, %u) for command '%c' comes from device[%s]>",
					datablock, addr0, addrn, fcode, this->device_description()->Data());

				this->delay_balance -= 1;

				if (this->delay_balance > 0) {
					syslog(Log::Notice, L"descard a delayed response from device[%s]", this->device_description()->Data());
				} else if (!this->confirmations.empty()) {
					this->apply_confirmation(fcode, datablock, addr0, addrn, this->data_pool->Data, datasize);
				}
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

void IMRMaster::apply_confirmation(size_t fcode, size_t db, size_t addr0, size_t addrn, uint8* data, size_t size) {
	if (fcode == this->preference.read_signal_fcode()) {
		if (db == this->preference.read_all_dbcode()) {
			for (auto confirmation : this->confirmations) {
				if (confirmation->available()) {
					confirmation->on_all_signals(addr0, addrn, data, size, this->get_logger());
				}
			}
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
		this->notify_connectivity_changed();
	}
}

void IMRMaster::notify_connectivity_changed() {
	bool connected = this->connected();

	for (auto lt = this->listeners.begin(); lt != this->listeners.end(); lt++) {
		(*lt)->on_plc_connectivity_changed(this, connected);
	}
}

/*************************************************************************************************/
void MRMaster::read_all_signal(uint16 data_block, uint16 addr0, uint16 addrn, float tidemark) {
	uint8 flbytes[] = { 0, 0, 0, 0 };
	size_t data_length = sizeof(flbytes) / sizeof(uint8);

	bigendian_float_set(flbytes, 0U, tidemark);

	this->request(this->preference.read_signal_fcode(), data_block, addr0, addrn, flbytes, data_length);
}

void MRMaster::write_analog_quantity(uint16 data_block, uint16 address, float datum) {
	uint8 flbytes[] = { 0, 0, 0, 0 };
	size_t data_length = sizeof(flbytes) / sizeof(uint8);

	bigendian_float_set(flbytes, 0U, datum);

	if (this->mode == PLCMasterMode::Release) {
		this->request(this->preference.write_analog_quantity_fcode(), data_block, address, address + 4U, flbytes, data_length);
	} else {
		this->logger->log_message(Log::Info, L"SET DB%d.DBD%d = %f", data_block, address, datum);
	}
}

void MRMaster::write_digital_quantity(uint16 data_block, uint8 idx, uint8 bidx, bool value) {
	uint8 xbytes[] = { 0, 0, 0, 0 };
	size_t data_length = sizeof(xbytes) / sizeof(uint8);

	xbytes[1] = idx;
	xbytes[2] = bidx;
	xbytes[3] = (value ? 0x01 : 0x00);

	if (this->mode == PLCMasterMode::Release) {
		this->request(this->preference.write_digital_quantity_fcode(), data_block, 0U, 0U, xbytes, data_length);
	} else {
		this->logger->log_message(Log::Info, L"EXE DB%d.DBX%d.%d", data_block, idx, bidx);
	}
}
