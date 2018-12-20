#pragma once

#include <list>

#include "syslog.hpp"

namespace WarGrey::SCADA {
	class IPLCMaster;

	private enum class PLCMasterMode { Debug, Root, User };

	private class IPLCStatusListener {
	public:
		virtual void on_plc_connectivity_changed(WarGrey::SCADA::IPLCMaster* master, bool connected) {}

	public:
		virtual void on_send_data(WarGrey::SCADA::IPLCMaster* master, long long bytes, double span_ms, double timestamp_ms) {}
		virtual void on_receive_data(WarGrey::SCADA::IPLCMaster* master, long long bytes, double span_ms, double timestamp_ms) {}
		virtual void on_confirm_data(WarGrey::SCADA::IPLCMaster* master, long long bytes, double span_ms, double timestamp_ms) {}
	};

	private class IPLCMaster abstract {
	public:
		virtual ~IPLCMaster() noexcept;

	public:
		virtual Platform::String^ device_hostname() = 0;
		virtual Platform::String^ device_description() = 0;

	public:
		virtual Syslog* get_logger() = 0;
		virtual void shake_hands() = 0;
		virtual bool connected() = 0;
		virtual void suicide() = 0;

	public:
		virtual void send_scheduled_request(long long count, long long interval, long long uptime) = 0;
		virtual bool authorized();

	public:
		void set_suicide_timeout(long long ms);
		void append_plc_status_listener(WarGrey::SCADA::IPLCStatusListener* listener);
		void notify_connectivity_changed();
		void notify_data_sent(long long bytes, double span_ms);
		void notify_data_received(long long bytes, double span_ms);
		void notify_data_confirmed(long long bytes, double span_ms);

	public:
		void set_mode(WarGrey::SCADA::PLCMasterMode mode);
		WarGrey::SCADA::PLCMasterMode get_mode();

	private:
		std::list<WarGrey::SCADA::IPLCStatusListener*> listeners;
		WarGrey::SCADA::PLCMasterMode mode = PLCMasterMode::Root;
		Platform::Object^ killer;
	};
}
