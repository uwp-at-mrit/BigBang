#include "modbus/client.hpp"
#include "modbus/protocol.hpp"
#include "rsyslog.hpp"

#include <ppltasks.h>

using namespace WarGrey::SCADA;

using namespace Concurrency;
using namespace Windows::Foundation;
using namespace Windows::Networking;
using namespace Windows::Networking::Sockets;
using namespace Windows::Storage::Streams;

/*************************************************************************************************/
IModbusClient::IModbusClient(Platform::String^ server, uint16 port, IModbusTransactionGenerator* generator) {
    this->target = ref new HostName(server);
    this->service = port.ToString();

    this->socket = ref new StreamSocket();
    this->socket->Control->KeepAlive = false;

	this->generator = generator;
	this->generator->reference();

    this->connect();
};

IModbusClient::~IModbusClient() {
	this->generator->destroy();
}

void IModbusClient::connect() {
	if (this->mbout != nullptr) {
		this->mbout = nullptr;
	}

    // TODO: It seems that this API is bullshit since exceptions may escape from async task.
    create_task(this->socket->ConnectAsync(this->target, this->service)).then([this](task<void> handshaking) {
        try {
            handshaking.get();

            this->mbin  = ref new DataReader(this->socket->InputStream);
            this->mbout = ref new DataWriter(this->socket->OutputStream);

            mbin->UnicodeEncoding = UnicodeEncoding::Utf8;
            mbin->ByteOrder = ByteOrder::BigEndian;
            mbout->UnicodeEncoding = UnicodeEncoding::Utf8;
            mbout->ByteOrder = ByteOrder::BigEndian;

            rsyslog(L">> %s:%s", this->target->RawName->Data(), this->service->Data());
        } catch (Platform::Exception^ e) {
            rsyslog(e->Message);
        }
    });
}

bool IModbusClient::is_connected() {
	return (this->mbout != nullptr);
}

uint8* IModbusClient::calloc_pdu() {
	return (uint8*)calloc(MODBUS_MAX_PDU_LENGTH, sizeof(uint8));
}

void IModbusClient::request(uint8 function_code, uint8* request, uint16 size, IModbusConfirmation* confirmation) {
	uint16 transaction = this->generator->yield();

	modbus_write_adu(this->mbout, transaction, 0x00, 0xFF, function_code, request, size);

	create_task(this->mbout->StoreAsync()).then([=](task<unsigned int> sending) {
		try {
			unsigned int sent = sending.get();
			
			if (this->debug) {
				rsyslog(L"[sent %u bytes to %s:%s]", sent, this->target->DisplayName->Data(), this->service->Data());
			}

			create_task(this->mbin->LoadAsync(MODBUS_MBAP_LENGTH)).then([=](unsigned int size) {
				uint16 transaction, protocol, length;
				uint8 unit;

				if (size < MODBUS_MBAP_LENGTH) {
					if (size == 0) {
						rsyslog(L"Server %s:%s has lost", this->target->RawName->Data(), this->service->Data());
					} else {
						rsyslog(L"MBAP header from server %s:%s is too short(%u < %hu)",
							this->target->RawName->Data(), this->service->Data(),
							size, MODBUS_MBAP_LENGTH);
					}

					cancel_current_task();
				}

				uint16 pdu_length = modbus_read_mbap(mbin, &transaction, &protocol, &length, &unit);

				return create_task(mbin->LoadAsync(pdu_length)).then([=](unsigned int size) {
					if (size < pdu_length) {
						rsyslog(L"PDU data from server %s:%s has been truncated(%u < %hu)",
							this->target->RawName->Data(), this->service->Data(),
							size, pdu_length);
						cancel_current_task();
					}

					uint8 function_code = mbin->ReadByte();

					if (this->debug) {
						rsyslog(L"[received indication(%hu, %hu, %hu, %hhu) for function 0x%02X from %s:%s]",
							transaction, protocol, length, unit, function_code,
							this->target->RawName->Data(), this->service->Data());
					}

					{ // clear dirty bytes
						int dirty = mbin->UnconsumedBufferLength;

						if (dirty > 0) {
							MODBUS_DISCARD_BYTES(mbin, dirty);
							if (this->debug) {
								rsyslog(L"[discarded last %d bytes of the confirmation from %s:%s]",
									this->target->RawName->Data(), this->service->Data());
								
							}
						}
					}
				});
			});
		} catch (task_canceled&) {
		} catch (Platform::Exception^ e) {
			rsyslog(e->Message);
			this->connect();
		}});
}

void IModbusClient::enable_debug(bool on_or_off) {
	this->debug = on_or_off;
}

bool IModbusClient::debug_enabled() {
	return this->debug;
}

/*************************************************************************************************/
int ModbusClient::read_coils(uint16 address, uint16 quantity, uint8* dest) { // MAP: Page 10
    return -MODBUS_EXN_DEVICE_BUSY;
}

int ModbusClient::write_coil(uint16 address, bool value, IModbusConfirmation* confirmation) { // MAP: Page 10
	if (this->is_connected()) {
		uint8* pdu_data = this->calloc_pdu();
		uint16 bvalue = (value ? 0xFF00 : 0x0000);

		MODBUS_SET_INT16_TO_INT8(request, 0, address);
		MODBUS_SET_INT16_TO_INT8(request, 2, bvalue);

		IModbusClient::request(MODBUS_WRITE_SINGLE_COIL, pdu_data, 4, confirmation);

	}
	
	return 0;
}

int ModbusClient::write_coils(uint16 address, uint16 quantity, uint8* dest, IModbusConfirmation* confirmation) { // MAP: Page 10
    return -MODBUS_EXN_MEMORY_PARITY_ERROR;
}

/*************************************************************************************************/
ModbusSequenceGenerator::ModbusSequenceGenerator(uint16 start) {
	this->start = ((start == 0) ? 1 : start);
	this->sequence = this->start;
}

void ModbusSequenceGenerator::reset() {
	this->sequence = this->start;
}

uint16 ModbusSequenceGenerator::yield() {
	uint16 seq = this->sequence;

	if (this->sequence == 0xFFFF) {
		this->reset();
	} else {
		this->sequence++;
	}

	return seq;
}
