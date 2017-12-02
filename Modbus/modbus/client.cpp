#include "modbus/client.hpp"
#include "rsyslog.hpp"

#include <ppltasks.h>

using namespace WarGrey::SCADA;

using namespace Concurrency;
using namespace Windows::Foundation;
using namespace Windows::Networking;
using namespace Windows::Networking::Sockets;
using namespace Windows::Storage::Streams;

/*************************************************************************************************/
IModbusClient::IModbusClient(Platform::String^ server, uint16 port) {
    this->target = ref new HostName(server);
    this->service = port.ToString();

    this->socket = ref new StreamSocket();
    this->socket->Control->KeepAlive = false;

    this->connect();
};

void IModbusClient::connect() {
    // TODO: It seems that this API is a bullshit since exceptions may escaped from async task.
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

/*************************************************************************************************/
int ModbusClient::read_coils(uint16 address, uint16 quantity, uint8* dest) { // MAP: Page 10
    return -MODBUS_EXN_DEVICE_BUSY;
}

int ModbusClient::write_coil(uint16 address, bool value) { // MAP: Page 10
    return 0;
}

int ModbusClient::write_coils(uint16 address, uint16 quantity, uint8* dest) { // MAP: Page 10
    return -MODBUS_EXN_MEMORY_PARITY_ERROR;
}