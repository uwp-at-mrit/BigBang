#pragma once

namespace WarGrey::SCADA {
    typedef Windows::Foundation::TypedEventHandler<
        Windows::Networking::Sockets::StreamSocketListener^,
        Windows::Networking::Sockets::StreamSocketListenerConnectionReceivedEventArgs^>
        TCPAcceptHandler;

    private ref class TCPListener sealed {
    public:
        TCPListener(unsigned short port);
        
    private:
        ~TCPListener();

    private:
        void welcome(
            Windows::Networking::Sockets::StreamSocketListener^ listener,
            Windows::Networking::Sockets::StreamSocketListenerConnectionReceivedEventArgs ^e);

        void read_stream_loop(
            Windows::Storage::Streams::IDataReader^ tcpin,
            Windows::Storage::Streams::IDataWriter^ tcpout,
            Windows::Networking::Sockets::StreamSocket^ plc);

    private:
        Windows::Networking::Sockets::StreamSocketListener^ listener;
    };
}
