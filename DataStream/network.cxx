#include "network.hxx"
#include "debug.hpp"

using namespace WarGrey::SCADA;

using namespace Windows::Foundation;
using namespace Windows::Networking::Sockets;
using namespace Windows::Storage::Streams;

TCPListener::TCPListener (unsigned short port) {
    this->listener = ref new StreamSocketListener();
    this->listener->Control->QualityOfService = SocketQualityOfService::LowLatency;
    this->listener->ConnectionReceived += ref new TCPAcceptHandler(this, &TCPListener::welcome);
    this->listener->BindEndpointAsync(nullptr, port.ToString());
}

TCPListener::~TCPListener() {}

void TCPListener::welcome(StreamSocketListener^ listener, StreamSocketListenerConnectionReceivedEventArgs^ e) {
    client = e->Socket;
    auto info = client->Information;
    auto tcpin = ref new DataReader(client->InputStream);
    auto tcpout = ref new DataWriter(client->OutputStream);

    tcpin->UnicodeEncoding = UnicodeEncoding::Utf8;
    tcpin->ByteOrder = ByteOrder::BigEndian;
    tcpout->UnicodeEncoding = UnicodeEncoding::Utf8;
    tcpout->ByteOrder = ByteOrder::BigEndian;

    trace(client->ToString() + ":" + tcpin->UnconsumedBufferLength.ToString());

    auto greetings = "Hello, "
        + info->RemoteAddress->DisplayName + ":"
        + info->RemotePort + " , I am "
        + info->LocalAddress->DisplayName + ":"
        + info->LocalPort + ".";

    trace(greetings);
    tcpout->WriteString(greetings);
    tcpout->WriteString("\n");
}
