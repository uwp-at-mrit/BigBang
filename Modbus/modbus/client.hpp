#pragma once

#include "modbus/constants.hpp"

namespace WarGrey::SCADA {
    private class IModbusClient {
    public:
        IModbusClient(Platform::String^ server, uint16 service);
        virtual ~IModbusClient() noexcept {};

    public: // data access
        virtual int read_coils(uint16 address, uint16 quantity, uint8* dest) = 0;
        virtual int write_coil(uint16 address, bool value) = 0;
        virtual int write_coils(uint16 address, uint16 quantity, uint8* dest) = 0;

    protected:
        void connect();

    protected:
        Windows::Storage::Streams::IDataReader^ mbin;
        Windows::Storage::Streams::IDataWriter^ mbout;

    private:
        Windows::Networking::Sockets::StreamSocket^ socket;
        Windows::Networking::HostName^ target;
        Platform::String^ service;
    };

    private class ModbusClient : public WarGrey::SCADA::IModbusClient {
    public:
        ModbusClient(Platform::String^ server, uint16 port = MODBUS_TCP_DEFAULT_PORT) : IModbusClient(server, port) {};

    public: // data access
        int read_coils(uint16 address, uint16 quantity, uint8* dest) override;
        int write_coil(uint16 address, bool value) override;
        int write_coils(uint16 address, uint16 quantity, uint8* dest) override;
    };
}
