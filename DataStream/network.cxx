#include "network.hxx"
#include "rsyslog.hpp"

#include <ppltasks.h>

using namespace WarGrey::SCADA;

using namespace Concurrency;
using namespace Windows::Foundation;
using namespace Windows::Networking::Sockets;
using namespace Windows::Storage::Streams;

PLC::PLC(StreamSocket^ client) {
    this->plc = plc;
    this->tcpin = ref new DataReader(client->InputStream);
    this->tcpout = ref new DataWriter(client->OutputStream);

    this->tcpin->UnicodeEncoding = UnicodeEncoding::Utf8;
    this->tcpin->ByteOrder = ByteOrder::BigEndian;
    this->tcpout->UnicodeEncoding = UnicodeEncoding::Utf8;
    this->tcpout->ByteOrder = ByteOrder::BigEndian;
}

void PLC::greetings() {
    this->tcpin->LoadAsync((unsigned int)4096);
    rsyslog(this->plc->ToString() + ":" + this->tcpin->UnconsumedBufferLength.ToString());
    
    auto greetings = "Hello, "
        + this->plc->Information->RemoteAddress->DisplayName + ":"
        + this->plc->Information->RemotePort + " , I am "
        + this->plc->Information->LocalAddress->DisplayName + ":"
        + this->plc->Information->LocalPort + ".";

    rsyslog(greetings);
    this->tcpout->WriteString(greetings);
    this->tcpout->WriteString("\n");
    this->tcpout->FlushAsync();
};

PLC::~PLC() {};

/*************************************************************************************************/
TCPListener::TCPListener (unsigned short port) {
    this->listener = ref new StreamSocketListener();
    this->listener->Control->QualityOfService = SocketQualityOfService::LowLatency;
    this->listener->Control->KeepAlive = false;
    create_task(this->listener->BindEndpointAsync(nullptr, port.ToString())).wait();
    this->listener->ConnectionReceived += ref new TCPAcceptHandler(this, &TCPListener::welcome);

    rsyslog(L"%s is ready!", L"Listener");
}

TCPListener::~TCPListener() {}

void TCPListener::welcome(StreamSocketListener^ listener, StreamSocketListenerConnectionReceivedEventArgs^ e) {
    this->client = ref new PLC(e->Socket);
    this->client->greetings();
}
