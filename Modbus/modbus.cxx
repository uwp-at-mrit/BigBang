#include "modbus.hxx"
#include "rsyslog.hpp"

#include <ppltasks.h>

using namespace WarGrey::SCADA;

using namespace Concurrency;
using namespace Windows::Foundation;
using namespace Windows::Networking::Sockets;
using namespace Windows::Storage::Streams;

/*************************************************************************************************/
ModbusListener::ModbusListener (unsigned short port) {
    this->listener = ref new StreamSocketListener();
    this->listener->Control->QualityOfService = SocketQualityOfService::LowLatency;
    this->listener->Control->KeepAlive = false;
    create_task(this->listener->BindEndpointAsync(nullptr, port.ToString())).then([this, port](task<void> binding) {
        try {
            binding.get();
            this->listener->ConnectionReceived += ref new TCPAcceptHandler(this, &ModbusListener::welcome);

            rsyslog(L"> 0.0.0.0:%u", port);
        } catch (Platform::Exception^ e) {
            rsyslog(e->Message);
        }
    });
}

ModbusListener::~ModbusListener() {}

void ModbusListener::welcome(StreamSocketListener^ listener, StreamSocketListenerConnectionReceivedEventArgs^ e) {
    auto tcpin = ref new DataReader(e->Socket->InputStream);
    auto tcpout = ref new DataWriter(e->Socket->OutputStream);

    tcpin->UnicodeEncoding = UnicodeEncoding::Utf8;
    tcpin->ByteOrder = ByteOrder::BigEndian;
    tcpout->UnicodeEncoding = UnicodeEncoding::Utf8;
    tcpout->ByteOrder = ByteOrder::BigEndian;

    this->read_stream_loop(tcpin, tcpout, e->Socket);
}

void ModbusListener::read_stream_loop(IDataReader^ tcpin, IDataWriter^ tcpout, StreamSocket^ plc) {
    unsigned int ssize = sizeof(unsigned int);
    create_task(tcpin->LoadAsync(ssize)).then([tcpin, ssize](unsigned int size) {
        if (size < ssize) {
            rsyslog("PLC has disconnected!");
            cancel_current_task();
        }
        
        unsigned int msgsize = tcpin->ReadUInt32();
        return create_task(tcpin->LoadAsync(msgsize)).then([ssize, msgsize](unsigned int size) {
            if (size < msgsize) {
                rsyslog("PLC is lost!");
                cancel_current_task();
            } else {
                rsyslog(L"[received %u + %u bytes]", ssize, msgsize);
            }});
    }).then([tcpout, plc](task<void> doReading) {
        try {
            doReading.get();
        } catch (Platform::Exception^ e) {
            rsyslog(e->Message);
            cancel_current_task();
        }

        Platform::String^ greetings = "Hello, "
            + plc->Information->RemoteAddress->DisplayName + ":"
            + plc->Information->RemotePort + ", I am "
            + plc->Information->LocalAddress->DisplayName + ":"
            + plc->Information->LocalPort + ".";
    
        tcpout->WriteString(greetings);
        tcpout->WriteString("\n");
        create_task(tcpout->StoreAsync()).then([greetings](task<unsigned int> doFlushing) {
            try {
                auto sent = doFlushing.get();
                rsyslog(greetings);
                rsyslog(L"[sent %u bytes]", sent);
            } catch (Platform::Exception^ e) {
                rsyslog(e->Message);
            }
        });
    });
}
