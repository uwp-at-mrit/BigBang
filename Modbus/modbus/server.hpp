#pragma once

#include "modbus/constants.hpp"

namespace WarGrey::SCADA {
    ref class ModbusListener;

    private class ModbusServer {
    public:
        ModbusServer(unsigned short port = MODBUS_TCP_DEFAULT_PORT);
        virtual ~ModbusServer() noexcept {};

    public:
        ModbusServer* listen();

    private:
        WarGrey::SCADA::ModbusListener^ listener;
    };
}
