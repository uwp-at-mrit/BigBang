#pragma once

#include "modbus/server.hpp"
#include "modbus/constants.hpp"

namespace WarGrey::SCADA {
    private class ModbusVirtualDevice : public WarGrey::SCADA::IModbusServer {
    public:
        ModbusVirtualDevice(uint16 port = MODBUS_TCP_DEFAULT_PORT) : IModbusServer(port) {};


    public: // data access
        int read_coils(uint16 address, uint16 quantity, uint8* dest) override;
        int write_coil(uint16 address, bool value) override;
        int write_coils(uint16 address, uint16 quantity, uint8* dest) override;

    public: // Diagnostics

    public: // Other

    private:
        uint16 input_bit0;
        uint16 input_bit_count;
        uint8* discrete_inputs;

    private:
        uint16 bit0;
        uint16 bit_count;
        uint8* coils;

    private:
        uint16 register0;
        uint16 register_count;
        uint16* holding_registers;

    private:
        uint16 input_register0;
        uint16 input_register_count;
        uint16* input_registers;
    };
}
