#pragma once

namespace WarGrey::SCADA {
    typedef Windows::Foundation::TypedEventHandler<
        Windows::Networking::Sockets::StreamSocketListener^,
        Windows::Networking::Sockets::StreamSocketListenerConnectionReceivedEventArgs^>
        TCPAcceptHandler;

    private ref class TCPListener sealed {
    public:
        TCPListener();
        
    private:
        ~TCPListener();

    private:
        void welcome(
            Windows::Networking::Sockets::StreamSocketListener^ sender,
            Windows::Networking::Sockets::StreamSocketListenerConnectionReceivedEventArgs ^e);

    private:
        Windows::Networking::Sockets::StreamSocketListener^ listener;
    };
}
