#pragma once

#include <list>

#include "mrit/magic.hpp"
#include "shared/stream.hpp"

#include "IPLCMaster.hpp"
#include "syslog.hpp"
#include "object.hpp"

namespace WarGrey::SCADA {
	class IMRMaster;

	private class IMRConfirmation abstract {
	public:
		virtual void on_realtime_data(float* db2, uint16 count, WarGrey::SCADA::Syslog* logger) = 0;
		virtual void on_forat_data(uint8* dqs, uint16 dqc, float* db20, uint16 count, WarGrey::SCADA::Syslog* logger) = 0;
		virtual void on_analog_input_data(float* db203, uint16 count, WarGrey::SCADA::Syslog* logger) = 0;
		virtual void on_raw_analog_input_data(float* db3, uint16 count, WarGrey::SCADA::Syslog* logger) = 0;
		virtual void on_analog_output_data(float* db204, uint16 count, WarGrey::SCADA::Syslog* logger) = 0;
		virtual void on_raw_analog_output_data(float* db5, uint16 count, WarGrey::SCADA::Syslog* logger) = 0;
		virtual void on_digital_input(uint8* db205, uint16 count, WarGrey::SCADA::Syslog* logger) = 0;
		virtual void on_raw_digital_input(uint8* db4, uint16 count, WarGrey::SCADA::Syslog* logger) = 0;
		virtual void on_raw_digital_output(uint8* db6, uint16 count, WarGrey::SCADA::Syslog* logger) = 0;
	};

	private class IMRMaster abstract : public WarGrey::SCADA::IPLCMaster, public WarGrey::SCADA::ISocketAcceptable {
    public:
        virtual ~IMRMaster() noexcept;

		IMRMaster(WarGrey::SCADA::Syslog* logger, Platform::String^ server, uint16 service, WarGrey::SCADA::IMRConfirmation* confirmation);

	public:
		Platform::String^ device_hostname() override;
		Platform::String^ device_description() override;

	public:
		Syslog* get_logger() override;
		void shake_hands() override;
		bool connected() override;

	public:
		void append_confirmation_receiver(WarGrey::SCADA::IMRConfirmation* confirmation);

    public:
		virtual void read_all_signal(uint16 data_block, uint16 addr0, uint16 addrn, float tidemark) = 0;
		virtual void write_analog_quantity(uint16 data_block, uint16 addr0, uint16 addrn) = 0;
		virtual void write_digital_quantity(uint16 data_block, uint16 addr0, uint16 addrn) = 0;

	public:
		void on_socket(Windows::Networking::Sockets::StreamSocket^ plc) override;

	protected:
		virtual bool fill_signal_preferences(uint16 type, uint16* count, uint16* addr0, uint16* addrn) = 0;
		virtual void on_all_signals(uint16 addr0, uint16 addrn, uint8* data, uint16 size) = 0;

	protected:
		void request(uint8 fcode, uint16 data_block, uint16 addr0, uint16 addrn, uint8* data, uint16 size);
		void set_message_alignment_size(uint16 size);

	private:
		void connect();
		void listen();
		void clear();
		void wait_process_confirm_loop();
		void apply_confirmation(uint8 code, uint16 db, uint16 addr0, uint16 addrn, uint8* data, uint16 size);

	protected:
		std::list<WarGrey::SCADA::IMRConfirmation*> confirmations;
		WarGrey::SCADA::Syslog* logger;

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
		uint16 alignment_size;
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
		void read_all_signal(uint16 data_block, uint16 addr0, uint16 addrn, float tidemark) override;

	public:
		void write_analog_quantity(uint16 data_block, uint16 addr0, uint16 addrn) override;
		void write_digital_quantity(uint16 data_block, uint16 addr0, uint16 addrn) override;

	protected:
		void on_all_signals(uint16 addr0, uint16 addrn, uint8* data, uint16 size) override;
	};

	private class MRConfirmation : public WarGrey::SCADA::IMRConfirmation {
	public:
		void on_realtime_data(float* db2, uint16 count, WarGrey::SCADA::Syslog* logger) override {}
		void on_forat_data(uint8* dqs, uint16 dqc, float* db20, uint16 count, WarGrey::SCADA::Syslog* logger) override {}
		void on_analog_input_data(float* db203, uint16 count, WarGrey::SCADA::Syslog* logger) override {}
		void on_raw_analog_input_data(float* db3, uint16 count, WarGrey::SCADA::Syslog* logger) override {}
		void on_analog_output_data(float* db204, uint16 count, WarGrey::SCADA::Syslog* logger) override {}
		void on_raw_analog_output_data(float* db5, uint16 count, WarGrey::SCADA::Syslog* logger) override {}
		void on_digital_input(uint8* db205, uint16 count, WarGrey::SCADA::Syslog* logger) override {}
		void on_raw_digital_input(uint8* db4, uint16 count, WarGrey::SCADA::Syslog* logger) override {}
		void on_raw_digital_output(uint8* db6, uint16 count, WarGrey::SCADA::Syslog* logger) override {}
	};
}
