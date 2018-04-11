#pragma once

#include <queue>
#include <mutex>

#include "modbus/codes.hpp"
#include "IPLCClient.hpp"
#include "syslog.hpp"
#include "object.hpp"

namespace WarGrey::SCADA {
	struct MRTransaction;
	class IMRClient;

	private class IMRConfirmation abstract {
	public:
		virtual void on_coils(uint16 transaction, uint16 address, uint8* coil_status, uint8 count, WarGrey::SCADA::Syslog* logger) = 0;
		virtual void on_discrete_inputs(uint16 transaction, uint16 address, uint8* input_status, uint8 count, WarGrey::SCADA::Syslog* logger) = 0;
		virtual void on_holding_registers(uint16 transaction, uint16 address, uint16* register_values, uint8 count, WarGrey::SCADA::Syslog* logger) = 0;
		virtual void on_input_registers(uint16 transaction, uint16 address, uint16* input_registers, uint8 count, WarGrey::SCADA::Syslog* logger) = 0;
		virtual void on_queue_registers(uint16 transaction, uint16 address, uint16* queue_registers, uint16 count, WarGrey::SCADA::Syslog* logger) = 0;

	public:
		virtual void on_echo_response(uint16 transaction, uint8 function_code, uint16 address, uint16 value, WarGrey::SCADA::Syslog* logger) = 0;
		virtual void on_echo_response(uint16 transaction, uint8 function_code, uint16 address, uint16 and, uint16 or, WarGrey::SCADA::Syslog* logger) = 0;
		virtual void on_exception(uint16 transaction, uint8 function_code, uint16 address0, uint8 reason, WarGrey::SCADA::Syslog* logger) = 0;

	public:
		virtual void on_private_response(uint16 transaction, uint8 function_code, uint8* data, uint8 count, WarGrey::SCADA::Syslog* logger) = 0;

	public:
		virtual void on_scheduled_request(IMRClient* device, long long count, long long interval, long long uptime) = 0;
	};

	private class IMRClient abstract : public WarGrey::SCADA::IPLCClient {
    public:
        virtual ~IMRClient() noexcept;

		IMRClient(WarGrey::SCADA::Syslog* logger,
			Platform::String^ server, uint16 service,
			WarGrey::SCADA::IMRConfirmation* confirmation);

	public:
		Platform::String^ device_hostname() override;
		Syslog* get_logger() override;
		bool connected() override;
		void send_scheduled_request(long long count, long long interval, long long uptime) override;

    public: // data access
		virtual uint16 read_coils(/*data(uint16 data_block, */uint16 addr0, uint16 addrn) = 0;
		virtual uint16 read_discrete_inputs(uint16 address, uint16 quantity) = 0;
        virtual uint16 write_coil(uint16 address, bool value) = 0;
        virtual uint16 write_coils(uint16 address, uint16 quantity, uint8* src) = 0;
		
		virtual uint16 read_holding_registers(uint16 address, uint16 quantity) = 0;
		virtual uint16 read_input_registers(uint16 address, uint16 quantity) = 0;
		virtual uint16 read_queues(uint16 address) = 0;
		virtual uint16 write_register(uint16 address, uint16 value) = 0;
		virtual uint16 write_registers(uint16 address, uint16 quantity, uint16* src) = 0;
		virtual uint16 mask_write_register(uint16 address, uint16 and, uint16 or) = 0;
		virtual uint16 write_read_registers(uint16 waddr, uint16 wquantity, uint16 raddr, uint16 rquantity, uint16* src) = 0;

	public: // Other
		virtual uint16 do_private_function(uint8 function_code, uint8* request, uint16 data_length) = 0;

	protected:
		uint16 request(MRTransaction* mt);
		uint16 do_simple_request(uint8 function_code, uint16 addr, uint16 val);
		uint16 do_write_registers(MRTransaction* mt, uint8 offset, uint16 address, uint16 quantity, uint16* src);

	private:
		void connect();
		void apply_request(MRTransaction* transaction);
		void wait_process_confirm_loop();

    private:
        Windows::Networking::Sockets::StreamSocket^ socket;
        Windows::Networking::HostName^ device;
        Platform::String^ service;

	private:
		Windows::Storage::Streams::DataReader^ mbin;
		Windows::Storage::Streams::DataWriter^ mbout;
		std::queue<MRTransaction*> blocking_requests;
		std::mutex blocking_section;

	private:
		WarGrey::SCADA::IMRConfirmation* confirmation;
		WarGrey::SCADA::Syslog* logger;
    };

    private class MRClient : public WarGrey::SCADA::IMRClient {
    public:
        MRClient(WarGrey::SCADA::Syslog* logger, Platform::String^ server, uint16 port = MODBUS_TCP_DEFAULT_PORT)
			: IMRClient(logger, server, port, nullptr) {};
		
		MRClient(WarGrey::SCADA::Syslog* logger, Platform::String^ server,
			IMRConfirmation* confirmation, uint16 port = MODBUS_TCP_DEFAULT_PORT)
			: IMRClient(logger, server, port, confirmation) {};

    public: // data access
		uint16 read_coils(uint16 address, uint16 quantity) override;
		uint16 read_discrete_inputs(uint16 address, uint16 quantity) override;
		uint16 write_coil(uint16 address, bool value) override;
        uint16 write_coils(uint16 address, uint16 quantity, uint8* dest) override;

		uint16 read_holding_registers(uint16 address, uint16 quantity) override;
		uint16 read_input_registers(uint16 address, uint16 quantity) override;
		uint16 read_queues(uint16 address) override;
		uint16 write_register(uint16 address, uint16 value) override;
		uint16 write_registers(uint16 address, uint16 quantity, uint16* src) override;
		uint16 mask_write_register(uint16 address, uint16 and, uint16 or) override;
		uint16 write_read_registers(uint16 waddr, uint16 wquantity, uint16 raddr, uint16 rquantity, uint16* src) override;

	public: // Others
		uint16 do_private_function(uint8 function_code, uint8* request, uint16 data_length) override;

	protected:
		uint8 request[MODBUS_MAX_PDU_LENGTH];
    };

	private class MRConfirmation : public WarGrey::SCADA::IMRConfirmation {
	public:
		void on_coils(uint16 transaction, uint16 address, uint8* coil_status, uint8 count, Syslog* logger) override {};
		void on_discrete_inputs(uint16 transaction, uint16 address, uint8* input_status, uint8 count, Syslog* logger) override {};
		void on_holding_registers(uint16 transaction, uint16 address, uint16* register_values, uint8 count, Syslog* logger) override {};
		void on_input_registers(uint16 transaction, uint16 address, uint16* input_registers, uint8 count, Syslog* logger) override {};
		void on_queue_registers(uint16 transaction, uint16 address, uint16* queue_registers, uint16 count, Syslog* logger) override {};

	public:
		void on_echo_response(uint16 transaction, uint8 function_code, uint16 address, uint16 value, Syslog* logger) override {};
		void on_echo_response(uint16 transaction, uint8 function_code, uint16 address, uint16 and, uint16 or, Syslog* logger) override {};
		void on_exception(uint16 transaction, uint8 function_code, uint16 address0, uint8 reason, Syslog* logger) override {};

	public:
		void on_private_response(uint16 transaction, uint8 function_code, uint8* data, uint8 count, Syslog* logger) override {};

	public:
		void on_scheduled_request(WarGrey::SCADA::IMRClient* device, long long count, long long interval, long long uptime) {};
	};
}
