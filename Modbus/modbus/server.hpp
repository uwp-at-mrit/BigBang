#pragma once

#include "modbus/constants.hpp"

namespace WarGrey::SCADA {
    ref class ModbusListener;

    private class IModbusServer {
    public:
        IModbusServer(uint16 port);
        virtual ~IModbusServer() noexcept {};

    public:
        IModbusServer* listen();
        int process(uint8 function_code, Windows::Storage::Streams::IDataReader^ mbin, uint8 *response);

    public: // data access
        virtual int read_coils(uint16 address, uint16 quantity, uint8* dest) = 0;
        virtual int write_coil(uint16 address, bool value) = 0;
        virtual int write_coils(uint16 address, uint16 quantity, uint8* dest) = 0;

    public: // Diagnostics

    public: // Other
        virtual int do_private_function(uint8 function_code, uint8* request, uint16 request_data_length, uint8* response);

    private:
        WarGrey::SCADA::ModbusListener^ listener;
    };

    private class ModbusServer : public WarGrey::SCADA::IModbusServer {
    public:
        ModbusServer(uint16 port = MODBUS_TCP_DEFAULT_PORT) : IModbusServer(port) {};

    public: // data access
        int read_coils(uint16 address, uint16 quantity, uint8* dest) override;
        int write_coil(uint16 address, bool value) override;
        int write_coils(uint16 address, uint16 quantity, uint8* dest) override;

    public: // Diagnostics

    public: // Other

    private:
        WarGrey::SCADA::ModbusListener^ listener;
    };
}
