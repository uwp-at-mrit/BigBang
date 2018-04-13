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

    public:
		virtual void read_data(uint16 data_block, uint16 addr0, uint16 addrn) = 0;
		virtual void write_analog_quantity(uint16 data_block, uint16 addr0, uint16 addrn) = 0;
		virtual void write_digital_quantity(uint16 data_block, uint16 addr0, uint16 addrn) = 0;

	protected:
		uint16 request(MRTransaction* mt);

	private:
		void connect();
		void apply_request(MRTransaction* transaction);
		void wait_process_confirm_loop();

    private:
        Windows::Networking::Sockets::StreamSocket^ socket;
        Windows::Networking::HostName^ device;
        Platform::String^ service;

	private:
		Windows::Storage::Streams::DataReader^ mrin;
		Windows::Storage::Streams::DataWriter^ mrout;
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
		void read_data(uint16 data_block, uint16 addr0, uint16 addrn) override;
		void write_analog_quantity(uint16 data_block, uint16 addr0, uint16 addrn) override;
		void write_digital_quantity(uint16 data_block, uint16 addr0, uint16 addrn) override;

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
