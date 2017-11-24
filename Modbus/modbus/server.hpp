#pragma once

#include "modbus/constants.hpp"

namespace WarGrey::SCADA {
    ref class ModbusListener;
    
    typedef Windows::Foundation::TypedEventHandler<
        Windows::Networking::Sockets::StreamSocketListener^,
        Windows::Networking::Sockets::StreamSocketListenerConnectionReceivedEventArgs^>
        TCPAcceptHandler;

    private class ModbusServer {
    public:
        ModbusServer(unsigned short port = MODBUS_TCP_DEFAULT_PORT);
        virtual ~ModbusServer() noexcept;
        ModbusServer* listen();

    private:
        WarGrey::SCADA::ModbusListener^ listener;
    };
}
