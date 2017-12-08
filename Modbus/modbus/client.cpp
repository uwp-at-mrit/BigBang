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

int ModbusClient::write_coil(uint16 address, bool value, IModbusExceptionListener* callback) { // MAP: Page 10
    return 0;
}

int ModbusClient::write_coils(uint16 address, uint16 quantity, uint8* dest, IModbusExceptionListener* callback) { // MAP: Page 10
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
