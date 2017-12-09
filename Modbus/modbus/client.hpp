#pragma once

#include "modbus/constants.hpp"
#include "object.hpp"

namespace WarGrey::SCADA {
	private class IModbusTransactionGenerator abstract : public WarGrey::SCADA::SharedObject {
	public:
		virtual void reset() = 0;
		virtual uint16 yield() = 0;
	};

	private class IModbusConfirmation abstract {
	public:
		virtual int on_exception(uint16 transaction, uint8 function_code, uint8 reason) = 0;
	};

    private class IModbusClient abstract {
    public:
        IModbusClient(Platform::String^ server, uint16 service, IModbusTransactionGenerator* generator);
        virtual ~IModbusClient() noexcept;

	public:
		void connect();
		uint8* calloc_pdu();
		void request(uint8 function_code, uint8* pdu_data, uint16 size, IModbusConfirmation* confirmation);
		void enable_debug(bool on_or_off);
		bool debug_enabled();

    public: // data access
        virtual int read_coils(uint16 address, uint16 quantity, uint8* dest) = 0;
        virtual int write_coil(uint16 address, bool value, IModbusConfirmation* confirmation) = 0;
        virtual int write_coils(uint16 address, uint16 quantity, uint8* dest, IModbusConfirmation* confirmation) = 0;

    protected:
        Windows::Storage::Streams::IDataReader^ mbin;
        Windows::Storage::Streams::IDataWriter^ mbout;

    private:
        Windows::Networking::Sockets::StreamSocket^ socket;
        Windows::Networking::HostName^ target;
        Platform::String^ service;

	private:
		WarGrey::SCADA::IModbusTransactionGenerator* generator;
		bool debug;
    };

	private class ModbusSequenceGenerator : public WarGrey::SCADA::IModbusTransactionGenerator {
	public:
		ModbusSequenceGenerator(uint16 start = 1);

	public:
		void reset() override;
		uint16 yield() override;

	private:
		uint16 start;
		uint16 sequence;
	};

    private class ModbusClient : public WarGrey::SCADA::IModbusClient {
    public:
        ModbusClient(Platform::String^ server, uint16 port = MODBUS_TCP_DEFAULT_PORT)
			: IModbusClient(server, port, new WarGrey::SCADA::ModbusSequenceGenerator()) {};

    public: // data access
        int read_coils(uint16 address, uint16 quantity, uint8* dest) override;
		int write_coil(uint16 address, bool value, IModbusConfirmation* confirmation) override;
        int write_coils(uint16 address, uint16 quantity, uint8* dest, IModbusConfirmation* confirmation) override;

	protected:
		uint8 request[MODBUS_MAX_PDU_LENGTH];
    };
}
