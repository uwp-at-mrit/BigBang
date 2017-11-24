#pragma once

namespace WarGrey::SCADA {
    typedef Windows::Foundation::TypedEventHandler<
        Windows::Networking::Sockets::StreamSocketListener^,
        Windows::Networking::Sockets::StreamSocketListenerConnectionReceivedEventArgs^>
        TCPAcceptHandler;

    private ref class ModbusListener sealed {
    public:
        ModbusListener(unsigned short port);
        
    private:
        ~ModbusListener();

    private:
        void welcome(
            Windows::Networking::Sockets::StreamSocketListener^ listener,
            Windows::Networking::Sockets::StreamSocketListenerConnectionReceivedEventArgs ^e);

    private:
        Windows::Networking::Sockets::StreamSocketListener^ listener;
    };
}
