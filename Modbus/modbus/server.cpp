#include <ppltasks.h>
#include <cstring>
#include <map>

#include "modbus/constants.hpp"
#include "modbus/server.hpp"
#include "modbus/protocol.hpp"
#include "modbus/exception.hpp"
#include "rsyslog.hpp"

using namespace WarGrey::SCADA;

using namespace Concurrency;
using namespace Windows::Foundation;
using namespace Windows::Networking::Sockets;
using namespace Windows::Storage::Streams;

#define MODBUS_CONFORMITY_LEVEL(id) ((id >= 0x80) ? 0x03 : ((id >= 0x03) ? 0x02 : 0x01))

static inline int modbus_echo(uint8* response, uint16 address, uint16 value) {
	MODBUS_SET_INT16_TO_INT8(response, 0, address);
	MODBUS_SET_INT16_TO_INT8(response, 2, value);
	return 4;
}

static inline int modbus_echo(uint8* response, uint16 address, uint16 value1, uint16 value2) {
	MODBUS_SET_INT16_TO_INT8(response, 0, address);
	MODBUS_SET_INT16_TO_INT8(response, 2, value1);
	MODBUS_SET_INT16_TO_INT8(response, 4, value2);
	return 6;
}

// delegate only accepts C++/CX class
private ref class ModbusListener sealed {
internal:
    ModbusListener(IModbusServer* server) : server(server) {}

public:
    void respond(StreamSocketListener^ listener, StreamSocketListenerConnectionReceivedEventArgs^ e) {
        StreamSocket^ client = e->Socket;
		StreamSocketInformation^ addr_info = client->Information;
		DataReader^ mbin = ref new DataReader(client->InputStream);
        DataWriter^ mbout = ref new DataWriter(client->OutputStream);
		uint8 pdu_data[MODBUS_TCP_MAX_ADU_LENGTH];
		Platform::String^ id = addr_info->RemoteHostName->RawName + ":" + addr_info->RemotePort;
	
		mbin->UnicodeEncoding = UnicodeEncoding::Utf8;
		mbin->ByteOrder = ByteOrder::BigEndian;
		mbout->UnicodeEncoding = UnicodeEncoding::Utf8;
		mbout->ByteOrder = ByteOrder::BigEndian;

		process(mbin, mbout, pdu_data, client, id);
    }

private:
	void process(DataReader^ mbin, DataWriter^ mbout, uint8* pdu_data, StreamSocket^ client, Platform::String^ id) {
		create_task(mbin->LoadAsync(MODBUS_MBAP_LENGTH)).then([=](unsigned int size) {
			uint16 transaction, protocol, length;
			uint8 unit;

			if (size < MODBUS_MBAP_LENGTH) {
				if (size == 0) {
					rsyslog(L"Client %s has disconnected", id->Data());
				} else {
					rsyslog(L"MBAP header from client %s is too short(%u < %hu)",
						id->Data(), size, MODBUS_MBAP_LENGTH);
				}

				cancel_current_task();
			}

			uint16 pdu_length = modbus_read_mbap(mbin, &transaction, &protocol, &length, &unit);

			return create_task(mbin->LoadAsync(pdu_length)).then([=](unsigned int size) {
				if (size < pdu_length) {
					rsyslog(L"PDU data from client %s has been truncated(%u < %hu)", id->Data(), size, pdu_length);
					cancel_current_task();
				}

				uint8 function_code = mbin->ReadByte();

				if (this->server->debug_enabled()) {
					rsyslog(L"[received indication(%hu, %hu, %hu, %hhu) for function 0x%02X from %s]",
						transaction, protocol, length, unit, function_code, id->Data());
				}

				int retcode
					= ((protocol == MODBUS_PROTOCOL) && (unit == MODBUS_TCP_SLAVE))
					? this->server->request(function_code, mbin, pdu_data)
					: -MODBUS_EXN_NEGATIVE_ACKNOWLEDGE;

				if (retcode >= 0) {
					modbus_write_adu(mbout, transaction, protocol, unit, function_code, pdu_data, retcode);
				} else {
					modbus_write_exn_adu(mbout, transaction, protocol, unit, function_code, (uint8)(-retcode));
				}

				{ // clear dirty bytes
					unsigned int dirty = mbin->UnconsumedBufferLength;

					if (dirty > 0) {
						MODBUS_DISCARD_BYTES(mbin, dirty);
						if (this->server->debug_enabled()) {
							rsyslog(L"[discarded last %u bytes of the indication from %s]", dirty, id->Data());
						}
					}
				}
			});
		}).then([=](task<void> doHandlingRequest) {
			try {
				doHandlingRequest.get();
				return create_task(mbout->StoreAsync());
			} catch (Platform::Exception^ e) {
				rsyslog(e->Message);
				cancel_current_task();
			} catch (task_canceled&) {
				rsyslog(L"Cancel dealing with the indication from %s", id->Data());
				cancel_current_task();
			}
		}).then([=](task<unsigned int> doReplying) {
			try {
				unsigned int sent = doReplying.get();

				if (this->server->debug_enabled()) {
					rsyslog(L"[sent %u-byte-response to %s]", sent, id->Data());
				}

				this->process(mbin, mbout, pdu_data, client, id);
			} catch (Platform::Exception^ e) {
				rsyslog(e->Message);
				delete client;
			} catch (task_canceled&) {
				rsyslog(L"Cancel responding to %s", id->Data());
				delete client;
			}
		});
	}

private:
    IModbusServer* server;
};

/*************************************************************************************************/
static std::map<int, ModbusListener^>* modbus_smart_listeners = nullptr;

IModbusServer::IModbusServer(uint16 port, const char* vendor, const char* code, const char* revision
	, const char* url, const char* name, const char* model, const char* appname)
	: debug(false), service(port.ToString()) {
	this->listener = ref new StreamSocketListener();
	this->listener->Control->QualityOfService = SocketQualityOfService::LowLatency;
	this->listener->Control->KeepAlive = false;
	this->listener->Control->NoDelay = true;

	this->standard_identifications[0x00] = vendor;
	this->standard_identifications[0x01] = code;
	this->standard_identifications[0x02] = revision;
	this->standard_identifications[0x03] = url;
	this->standard_identifications[0x04] = name;
	this->standard_identifications[0x05] = model;
	this->standard_identifications[0x06] = appname;
};

IModbusServer::~IModbusServer() {
	auto uuid = this->listener->GetHashCode();

	if (modbus_smart_listeners != nullptr) {
		modbus_smart_listeners->erase(uuid);

		if (modbus_smart_listeners->empty()) {
			delete modbus_smart_listeners;
			modbus_smart_listeners = nullptr;
		}
	}
}

void IModbusServer::listen() {
	auto waitor = ref new ModbusListener(this);
	auto uuid = this->listener->GetHashCode();

	if (modbus_smart_listeners == nullptr) {
		modbus_smart_listeners = new std::map<int, ModbusListener^>();
	}

	modbus_smart_listeners->insert(std::pair<int, ModbusListener^>(uuid, waitor));
	
	try {
		create_task(this->listener->BindEndpointAsync(nullptr, this->service)).wait();
		
		this->listener->ConnectionReceived
			+= ref new TypedEventHandler<StreamSocketListener^, StreamSocketListenerConnectionReceivedEventArgs^>(
				waitor,
				&ModbusListener::respond);

		rsyslog(L"## 0.0.0.0:%s", this->service->Data());
	} catch (Platform::Exception^ e) {
		rsyslog(e->Message);
	}
}

void IModbusServer::enable_debug(bool on_or_off) {
    this->debug = on_or_off;
}

bool IModbusServer::debug_enabled() {
    return this->debug;
}

int IModbusServer::request(uint8 funcode, DataReader^ mbin, uint8 *response) { // MAP: Page 10
	int retcode = -MODBUS_EXN_DEVICE_FAILURE;

    switch (funcode) {
	case MODBUS_READ_COILS: case MODBUS_READ_DISCRETE_INPUTS: { // MAP: Page 11, 12
        uint16 address = mbin->ReadUInt16();
		uint16 quantity = mbin->ReadUInt16();

        if ((quantity < 0x01) || (quantity > MODBUS_MAX_READ_BITS)) {
            retcode = -modbus_illegal_data_value(quantity, 0x01, MODBUS_MAX_READ_BITS, this->debug);
		} else if (funcode == MODBUS_READ_COILS) {
			retcode = this->read_coils(address, quantity, response + 1);
		} else {
			retcode = this->read_discrete_inputs(address, quantity, response + 1);
		}
			
		if (retcode >= 0) {
			response[0] = (uint8)retcode;
			retcode += 1;
		}
	}; break;
	case MODBUS_READ_HOLDING_REGISTERS: case MODBUS_READ_INPUT_REGISTERS: { // MAP: Page 15, 16
		uint16 address = mbin->ReadUInt16();
		uint16 quantity = mbin->ReadUInt16();

		if ((quantity < 0x01) || (quantity > MODBUS_MAX_READ_REGISTERS)) {
			retcode = -modbus_illegal_data_value(quantity, 0x01, MODBUS_MAX_READ_REGISTERS, this->debug);
		} else if (funcode == MODBUS_READ_HOLDING_REGISTERS) {
			retcode = this->read_holding_registers(address, quantity, response + 1);
		} else {
			retcode = this->read_input_registers(address, quantity, response + 1);
		}

		if (retcode >= 0) {
			response[0] = (uint8)retcode;
			retcode += 1;
		}
	}; break;
	case MODBUS_WRITE_SINGLE_COIL: case MODBUS_WRITE_SINGLE_REGISTER: { // MAP: Page 17, 19
        uint16 address = mbin->ReadUInt16();
        uint16 value = mbin->ReadUInt16();
        
		if (funcode == MODBUS_WRITE_SINGLE_COIL) {
			switch (value) {
			case 0x0000: retcode = this->write_coil(address, false); break;
			case 0xFF00: retcode = this->write_coil(address, true); break;
			default: retcode = -modbus_illegal_enum_value(value, 0xFF00, 0x0000, this->debug); break;
			}
		} else {
			retcode = this->write_register(address, value);
		}

        if (retcode >= 0) {
            retcode = modbus_echo(response, address, value);
        }
	}; break;
	case MODBUS_WRITE_MULTIPLE_COILS: case MODBUS_WRITE_MULTIPLE_REGISTERS: { // MAP: Page 29, 30
		uint16 address = mbin->ReadUInt16();
        uint16 quantity = mbin->ReadUInt16();
        uint8 count = mbin->ReadByte();
        
		MODBUS_READ_BYTES(mbin, response, count);
		
		if (funcode == MODBUS_WRITE_MULTIPLE_COILS) {
			if ((quantity < 0x01) || (quantity > MODBUS_MAX_WRITE_BITS)) {
				retcode = -modbus_illegal_data_value(quantity, 0x01, MODBUS_MAX_WRITE_BITS, this->debug);
			} else if (count != MODBUS_COIL_NStar(quantity)) {
				retcode = -modbus_illegal_data_value(count, MODBUS_COIL_NStar(quantity), this->debug);
			} else {
				retcode = this->write_coils(address, quantity, response);
			}
		} else {
			if ((quantity < 0x01) || (quantity > MODBUS_MAX_WRITE_REGISTERS)) {
				retcode = -modbus_illegal_data_value(quantity, 0x01, MODBUS_MAX_WRITE_REGISTERS, this->debug);
			} else if (count != MODBUS_REGISTER_NStar(quantity)) {
				retcode = -modbus_illegal_data_value(count, MODBUS_REGISTER_NStar(quantity), this->debug);
			} else {
				retcode = this->write_registers(address, quantity, response);
			}
		}

		if (retcode >= 0) {
			// TODO: is echoing right here?
			retcode = modbus_echo(response, address, quantity);
		}
	}; break;
	case MODBUS_READ_FILE_RECORD: case MODBUS_WRITE_FILE_RECORD: { // MAP: Page 32, 34
		retcode = -MODBUS_EXN_DEVICE_FAILURE;
	}; break;
	case MODBUS_MASK_WRITE_REGISTER: { // MAP: Page 36
		uint16 address = mbin->ReadUInt16();
		uint16 mand = mbin->ReadUInt16();
		uint16 mor = mbin->ReadUInt16();

		retcode = this->mask_write_register(address, mand, mor);

		if (retcode >= 0) {
			retcode = modbus_echo(response, address, mand, mor);
		}
	}; break;
	case MODBUS_WRITE_AND_READ_REGISTERS: { // MAP: Page 38
		uint16 raddress = mbin->ReadUInt16();
		uint16 rquantity = mbin->ReadUInt16();
		uint16 waddress = mbin->ReadUInt16();
		uint16 wquantity = mbin->ReadUInt16();
		uint8 wcount = mbin->ReadByte();
		uint8* rwpool = response + 1;

		MODBUS_READ_BYTES(mbin, rwpool, wcount);

		if ((rquantity < 0x01) || (rquantity > MODBUS_MAX_WR_READ_REGISTERS)) {
			retcode = -modbus_illegal_data_value(rquantity, 0x01, MODBUS_MAX_WR_READ_REGISTERS, this->debug);
		} else if ((wquantity < 0x01) || (wquantity > MODBUS_MAX_WR_WRITE_REGISTERS)) {
			retcode = -modbus_illegal_data_value(wquantity, 0x01, MODBUS_MAX_WR_WRITE_REGISTERS, this->debug);
		} else if (wcount != MODBUS_REGISTER_NStar(wquantity)) {
			retcode = -modbus_illegal_data_value(wcount, MODBUS_REGISTER_NStar(wquantity), this->debug);
		} else {
			retcode = this->write_read_registers(waddress, wquantity, raddress, rquantity, rwpool);
		}

		if (retcode >= 0) {
			response[0] = (uint8)retcode;
			retcode += 1;
		}
	}; break;
	case MODBUS_READ_FIFO_QUEUES: { // MAP: Page 40
		uint16 address = mbin->ReadUInt16();
		retcode = this->read_fifo_queues(address, response + 4);

		if (retcode >= 0) {
			if (retcode = 31) {
				retcode = -modbus_illegal_data_value(retcode, 0, 32, this->debug);
			} else {
				uint8 NStar = MODBUS_QUEUE_NStar(retcode);
				retcode = NStar + modbus_echo(response, NStar + 2, retcode);
			}
		}
	}; break;
		/* Following functions (before next `break`) are Serial Line Only */
	case MODBUS_READ_EXCEPTION_STATUS: case MODBUS_DIAGNOSTIC: { // MAP: Page 20, 21
	}
	case MODBUS_GET_COM_EVENT_COUNTER: case MODBUS_GET_COM_EVENT_LOG: { // MAP: Page 25, 26
	};
	case MODBUS_REPORT_SLAVE_ID: { // MAP: Page 31
		retcode = -MODBUS_EXN_NEGATIVE_ACKNOWLEDGE;
	}; break;
	case MODBUS_READ_DEVICE_IDENTIFICATION: { // MAP: Page 41-
		uint8 MEI_type = mbin->ReadByte();

		switch (MEI_type) {
		case 0x0D: { // MAP: Page 42
			retcode = -MODBUS_EXN_DEVICE_BUSY;
		}; break;
		case 0x0E: { // MAP: Page 43
			uint8 read_device_id_code = mbin->ReadByte();
			uint8 object_id = mbin->ReadByte();
			uint8 conformity_level = read_device_id_code;
			uint8 more_follows = 0x00;
			uint8 object_count = 0x00;
			uint8 object_list_idx = 6;
			uint8 capacity = MODBUS_MAX_PDU_LENGTH - object_list_idx;
			uint8* object_list = response + object_list_idx;
			uint8 object_stdmax = sizeof(standard_identifications) / sizeof(char*);
			uint8 object_max = 0xFF;

			if ((read_device_id_code < 0x01) || (read_device_id_code > 0x04)) {
				retcode = -modbus_illegal_data_value(read_device_id_code, 0x01, 0x04, this->debug);
			} else {
				int object_used = this->process_device_identification(object_list, object_id, capacity, true);
				bool stream_access = (read_device_id_code != 0x04);
				
				if ((object_used < 0) && stream_access) {
					object_id = 0x00;
					object_used = this->process_device_identification(object_list, object_id, capacity, true);
				}

				conformity_level = MODBUS_CONFORMITY_LEVEL(object_id);
				switch (conformity_level) {
				case 0x01: object_max = 0x02; break;
				case 0x02: object_max = object_stdmax - 1; break;
				default: object_max = 0xFF; break;
				}

				retcode = 0;
				while (object_used > 0) {
					object_list = object_list + object_used;
					object_count++;
					capacity -= object_used;
					retcode += object_used;

					object_used
						= (stream_access && (object_id + object_count <= object_max))
						? this->process_device_identification(object_list, object_id + object_count, capacity, false)
						: -1;
				}

				// TODO: nullptr may be left before non-null identifications.
				more_follows = ((object_used < 0) ? 0x00 : 0xFF);
			}

			if (retcode == 0) {
				retcode = -modbus_identification_not_found(object_id, 0x00, object_stdmax - 1, 0x80, this->debug);
			} else if (retcode > 0) { // implies object_count > 0;
				response[0] = MEI_type;
				response[1] = read_device_id_code;
				response[2] = conformity_level;
				response[3] = more_follows;
				response[4] = (more_follows == 0x00) ?  0x00 : (object_id + object_count);
				response[5] = object_count;
				retcode += object_list_idx;
			}
		}; break;
		default: {
			retcode = -modbus_illegal_enum_value(MEI_type, 0x0D, 0x0E, this->debug);
		}
		}
	}; break;
    default: {
        uint8 request[MODBUS_TCP_MAX_ADU_LENGTH];
        int data_length = mbin->UnconsumedBufferLength;

        MODBUS_READ_BYTES(mbin, request, data_length);

        retcode = this->do_private_function(funcode, request, data_length, response);
    };
    }

	return retcode;
}

int IModbusServer::process_device_identification(uint8* object_list, uint8 object, uint8 capacity, bool cut) {
	int used = -1; 
	const char* strval = (object < 0x07)
		? this->standard_identifications[object]
		: this->access_private_device_identification(object);

	if (strval != nullptr) {
		uint8 size = (uint8)strlen(strval);

		used = size + 2;
		if (used > capacity) {
			if (cut && (capacity > 2)) {
				size = capacity - 2;
				used = capacity;
			} else {
				used = 0;
			}
		}

		if (used > 0) {
			object_list[0] = object;
			object_list[1] = size;
			memcpy(object_list + 2, strval, size);
			
			if (this->debug) {
				rsyslog(L"[device identification object 0x%02X(%u:%u bytes) is ready]", object, size, capacity);
			}
		} else {
			if (this->debug) {
				rsyslog(L"[device identification object 0x%02X will be sent in the next transcation]", object);
			}
		}
	}

	return used;
}

int IModbusServer::do_private_function(uint8 function_code, uint8* request, uint16 data_length, uint8* response) { // MAP: Page 10
    return -modbus_illegal_function(function_code, this->debug);
}
