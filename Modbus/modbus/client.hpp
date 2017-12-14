#pragma once

#include <map>
#include <queue>

#include "modbus/constants.hpp"
#include "object.hpp"

namespace WarGrey::SCADA {
	struct ModbusTransaction;

	private class IModbusTransactionIdGenerator abstract : public WarGrey::SCADA::SharedObject {
	public:
		virtual void reset() = 0;
		virtual uint16 yield() = 0;
	};

	private class IModbusConfirmation abstract {
	public:
		virtual void on_coils(uint16 transaction, uint8* coil_status, uint8 count) = 0;
		virtual void on_discrete_inputs(uint16 transaction, uint8* input_status, uint8 count) = 0;
		virtual void on_holding_registers(uint16 transaction, uint16* register_values, uint8 count) = 0;
		virtual void on_input_registers(uint16 transaction, uint16* input_registers, uint8 count) = 0;

		virtual void on_echo_response(uint16 transaction, uint8 function_code, uint16 address, uint16 value) = 0;
		virtual void on_echo_response(uint16 transaction, uint8 function_code, uint16 address, uint16 and, uint16 or) = 0;
		virtual void on_exception(uint16 transaction, uint8 function_code, uint8 reason) = 0;
	};

    private class IModbusClient abstract {
    public:
        IModbusClient(Platform::String^ server, uint16 service, IModbusTransactionIdGenerator* generator);
        virtual ~IModbusClient() noexcept;

	public:
		uint8* calloc_pdu();
		uint16 request(uint8 function_code, uint8* pdu_data, uint16 size, IModbusConfirmation* confirmation);
		void enable_debug(bool on_or_off);
		bool debug_enabled();

    public: // data access
		virtual uint16 read_coils(uint16 address, uint16 quantity, IModbusConfirmation* confirmation) = 0;
		virtual uint16 read_discrete_inputs(uint16 address, uint16 quantity, IModbusConfirmation* confirmation) = 0;
        virtual uint16 write_coil(uint16 address, bool value, IModbusConfirmation* confirmation) = 0;
        virtual uint16 write_coils(uint16 address, uint16 quantity, uint8* src, IModbusConfirmation* confirmation) = 0;
		
		virtual uint16 read_holding_registers(uint16 address, uint16 quantity, IModbusConfirmation* confirmation) = 0;
		virtual uint16 read_input_registers(uint16 address, uint16 quantity, IModbusConfirmation* confirmation) = 0;
		virtual uint16 write_register(uint16 address, uint16 value, IModbusConfirmation* confirmation) = 0;
		virtual uint16 write_registers(uint16 address, uint16 quantity, uint16* src, IModbusConfirmation* confirmation) = 0;
		virtual uint16 mask_write_register(uint16 address, uint16 and, uint16 or, IModbusConfirmation* confirmation) = 0;
		
		virtual uint16 write_read_registers(
			uint16 waddr, uint16 wquantity,
			uint16 raddr, uint16 rquantity, uint16* src,
			IModbusConfirmation* confirmation) = 0;

    protected:
        Windows::Storage::Streams::DataReader^ mbin;
        Windows::Storage::Streams::DataWriter^ mbout;

	private:
		void connect();
		void apply_request(std::pair<uint16, ModbusTransaction>& transaction);
		void wait_process_callback_loop();

    private:
        Windows::Networking::Sockets::StreamSocket^ socket;
        Windows::Networking::HostName^ device;
        Platform::String^ service;

	private:
		WarGrey::SCADA::IModbusTransactionIdGenerator* generator;
		std::map<uint16, ModbusTransaction>* blocking_requests;
		std::map<uint16, ModbusTransaction>* pending_requests;
		std::queue<uint8*>* pdu_pool;
		bool debug;
    };

	private class ModbusSequenceGenerator : public WarGrey::SCADA::IModbusTransactionIdGenerator {
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
		uint16 read_coils(uint16 address, uint16 quantity, IModbusConfirmation* confirmation) override;
		uint16 read_discrete_inputs(uint16 address, uint16 quantity, IModbusConfirmation* confirmation) override;
		uint16 write_coil(uint16 address, bool value, IModbusConfirmation* confirmation) override;
        uint16 write_coils(uint16 address, uint16 quantity, uint8* dest, IModbusConfirmation* confirmation) override;


		uint16 read_holding_registers(uint16 address, uint16 quantity, IModbusConfirmation* confirmation) override;
		uint16 read_input_registers(uint16 address, uint16 quantity, IModbusConfirmation* confirmation) override;
		uint16 write_register(uint16 address, uint16 value, IModbusConfirmation* confirmation) override;
		uint16 write_registers(uint16 address, uint16 quantity, uint16* src, IModbusConfirmation* confirmation) override;
		uint16 mask_write_register(uint16 address, uint16 and, uint16 or, IModbusConfirmation* confirmation) override;
		
		uint16 write_read_registers(
			uint16 waddr, uint16 wquantity,
			uint16 raddr, uint16 rquantity, uint16* src,
			IModbusConfirmation* confirmation) override;

	protected:
		uint8 request[MODBUS_MAX_PDU_LENGTH];
    };

	private class ModbusConfirmation : public WarGrey::SCADA::IModbusConfirmation {
	public:
		void on_coils(uint16 transaction, uint8* coil_status, uint8 count) override {};
		void on_discrete_inputs(uint16 transaction, uint8* input_status, uint8 count) override {};
		void on_holding_registers(uint16 transaction, uint16* register_values, uint8 count) override {};
		void on_input_registers(uint16 transaction, uint16* input_registers, uint8 count) override {};

		void on_echo_response(uint16 transaction, uint8 function_code, uint16 address, uint16 value) override {};
		void on_echo_response(uint16 transaction, uint8 function_code, uint16 address, uint16 and, uint16 or) override {};
		void on_exception(uint16 transaction, uint8 function_code, uint8 reason) override {};
	};
}
