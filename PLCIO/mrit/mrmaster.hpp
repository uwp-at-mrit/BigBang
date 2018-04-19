#pragma once

#include <list>

#include "mrit/magic.hpp"
#include "shared/stream.hpp"

#include "IPLCMaster.hpp"
#include "syslog.hpp"
#include "object.hpp"

namespace WarGrey::SCADA {
	class IMRMaster;

	private enum class MRSignal {
		Realtime /* it must be the first, @see IMRMaster::IMRMaster() */,
		DigitalInputRaw, DigitalOutputRaw, DigitalInput,
		AnalogInput, AnalogOutput, AnalogInputRaw, AnalogOutputRaw,
		All /* it must be the last, @see IMRMaster::IMRMaster() */
	};

	private class IMRConfirmation abstract {
	public:
		virtual void on_realtime_data(uint16 address, uint16* input_registers, uint8 count, WarGrey::SCADA::Syslog* logger) = 0;
		virtual void on_analog_input_data(uint16 address, uint8* coil_status, uint8 count, WarGrey::SCADA::Syslog* logger) = 0;
		virtual void on_analog_output_data(uint16 address, uint8* input_status, uint8 count, WarGrey::SCADA::Syslog* logger) = 0;
		virtual void on_digital_input_data(uint16 address, uint16* register_values, uint8 count, WarGrey::SCADA::Syslog* logger) = 0;
		virtual void on_digital_output_data(uint16 address, uint16* input_registers, uint8 count, WarGrey::SCADA::Syslog* logger) = 0;
	};

	private class IMRMaster abstract : public WarGrey::SCADA::IPLCMaster, public WarGrey::SCADA::ISocketAcceptable {
    public:
        virtual ~IMRMaster() noexcept;

		IMRMaster(WarGrey::SCADA::Syslog* logger, Platform::String^ server, uint16 service, WarGrey::SCADA::IMRConfirmation* confirmation);

	public:
		Platform::String^ device_hostname() override;
		Syslog* get_logger() override;
		void shake_hands() override;
		bool connected() override;

	public:
		void append_confirmation_receiver(WarGrey::SCADA::IMRConfirmation* confirmation);

    public:
		virtual void read_all_signal(uint16 data_block, uint16 addr0, uint16 addrn) = 0;
		virtual void write_analog_quantity(uint16 data_block, uint16 addr0, uint16 addrn) = 0;
		virtual void write_digital_quantity(uint16 data_block, uint16 addr0, uint16 addrn) = 0;

	public:
		void on_socket(Windows::Networking::Sockets::StreamSocket^ plc) override;

	protected:
		virtual bool fill_signal_preferences(MRSignal type, uint16 *data_block, uint16* addr0, uint16* addrn) = 0;

	protected:
		void request(uint8 fcode, uint16 data_block, uint16 addr0, uint16 addrn, uint8* data, uint16 size);

	private:
		void connect();
		void listen();
		void clear();
		void wait_process_confirm_loop();

    private:
		// NOTE: Either `listener` or `socket` will work depends on the `device`.
		WarGrey::SCADA::StreamListener* listener;
        Windows::Networking::Sockets::StreamSocket^ socket;
        Windows::Networking::HostName^ device;
        Platform::String^ service;

	private:
		Windows::Storage::Streams::DataReader^ mrin;
		Windows::Storage::Streams::DataWriter^ mrout;
		
	private:
		std::list<WarGrey::SCADA::IMRConfirmation*> confirmations;
		WarGrey::SCADA::Syslog* logger;
    };

    private class MRMaster : public WarGrey::SCADA::IMRMaster {
    public:
        MRMaster(WarGrey::SCADA::Syslog* logger, Platform::String^ server = nullptr, uint16 port = MR_TCP_DEFAULT_PORT)
			: IMRMaster(logger, server, port, nullptr) {}
		
		MRMaster(WarGrey::SCADA::Syslog* logger, IMRConfirmation* confirmation, Platform::String^ server = nullptr, uint16 port = MR_TCP_DEFAULT_PORT)
			: IMRMaster(logger, server, port, confirmation) {}

	public:
		void send_scheduled_request(long long count, long long interval, long long uptime) {}

	public:
		void read_all_signal(uint16 data_block, uint16 addr0, uint16 addrn) override;

	public:
		void write_analog_quantity(uint16 data_block, uint16 addr0, uint16 addrn) override;
		void write_digital_quantity(uint16 data_block, uint16 addr0, uint16 addrn) override;
	};

	private class MRConfirmation : public WarGrey::SCADA::IMRConfirmation {
	public:
		void on_realtime_data(uint16 address, uint16* input_registers, uint8 count, WarGrey::SCADA::Syslog* logger) override {}
		void on_analog_input_data(uint16 address, uint8* coil_status, uint8 count, WarGrey::SCADA::Syslog* logger) override {}
		void on_analog_output_data(uint16 address, uint8* input_status, uint8 count, WarGrey::SCADA::Syslog* logger) override {}
		void on_digital_input_data(uint16 address, uint16* register_values, uint8 count, WarGrey::SCADA::Syslog* logger) override {}
		void on_digital_output_data(uint16 address, uint16* input_registers, uint8 count, WarGrey::SCADA::Syslog* logger) override {}
	};
}
