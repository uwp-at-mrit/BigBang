#pragma once

#include <list>
#include <cinttypes>

#include "mrit/message.hpp"
#include "shared/stream.hpp"

#include "IPLCMaster.hpp"
#include "syslog.hpp"
#include "object.hpp"

namespace WarGrey::SCADA {
	class IMRMaster;

	private class IMRConfirmation abstract {
	public:
		virtual bool available() { return true; }

	public:
		virtual void on_all_signals(size_t addr0, size_t addrn, uint8* data, size_t size, WarGrey::SCADA::Syslog* logger) = 0;
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
		void suicide() override;

	public:
		void append_confirmation_receiver(WarGrey::SCADA::IMRConfirmation* confirmation);

    public:
		virtual void read_all_signal(uint16 data_block, uint16 addr0, uint16 addrn, float tidemark = 0.0F) = 0;
		virtual void write_analog_quantity(uint16 data_block, uint16 address, float datum) = 0;
		virtual void write_digital_quantity(uint16 data_block, uint8 index, uint8 bit_index, bool value = true) = 0;

	public:
		void on_socket(Windows::Networking::Sockets::StreamSocket^ plc) override;
		
	protected:
		void request(size_t fcode, size_t data_block, size_t addr0, size_t addrn, uint8* data, size_t size);
		void set_message_preference(WarGrey::SCADA::MrMessageConfiguration& config);

	private:
		void connect();
		void listen();
		void clear();
		void wait_process_confirm_loop();
		void apply_confirmation(size_t code, size_t db, size_t addr0, size_t addrn, uint8* data, size_t size);

	protected:
		std::list<WarGrey::SCADA::IMRConfirmation*> confirmations;
		WarGrey::SCADA::MrMessageConfiguration preference;
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
		uint8* data_pool;
		unsigned int pool_size;
    };

    private class MRMaster : public WarGrey::SCADA::IMRMaster {
    public:
        MRMaster(WarGrey::SCADA::Syslog* logger, Platform::String^ server, uint16 port, IMRConfirmation* confirmation = nullptr)
			: IMRMaster(logger, server, port, confirmation) {}

		MRMaster(WarGrey::SCADA::Syslog* logger, uint16 port, IMRConfirmation* confirmation = nullptr)
			: MRMaster(logger, nullptr, port, confirmation) {}

	public:
		void send_scheduled_request(long long count, long long interval, long long uptime) {}

	public:
		void read_all_signal(uint16 data_block, uint16 addr0, uint16 addrn, float tidemark = 0.0F) override;

	public:
		void write_analog_quantity(uint16 data_block, uint16 address, float datum) override;
		void write_digital_quantity(uint16 data_block, uint8 index, uint8 bit_index, bool value = true) override;
	};

	private class MRConfirmation : public WarGrey::SCADA::IMRConfirmation {
	public:
		void on_all_signals(size_t addr0, size_t addrn, uint8* data, size_t size, WarGrey::SCADA::Syslog* logger) override {}
	};
}
