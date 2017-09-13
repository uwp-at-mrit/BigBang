#include "network.hpp"

using namespace WarGrey::SCADA;

using namespace Windows::Foundation;
using namespace Windows::Networking::Sockets;

TCPListener::TCPListener () {
    this->listener = ref new StreamSocketListener();
    this->listener->Control->QualityOfService = SocketQualityOfService::LowLatency;
    this->listener->ConnectionReceived += ref new TCPAcceptHandler(this, &TCPListener::welcome);
}

TCPListener::~TCPListener() {}

void TCPListener::welcome(StreamSocketListener^ sender, StreamSocketListenerConnectionReceivedEventArgs^ e) {

}