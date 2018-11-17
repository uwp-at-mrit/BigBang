#pragma once

#include <list>

#include "syslog.hpp"

namespace WarGrey::SCADA {
	class IPLCMaster;

	private enum class PLCMasterMode { Debug, Release };

	private class IPLCStatusListener {
	public:
		virtual void on_plc_connectivity_changed(WarGrey::SCADA::IPLCMaster* master, bool connected) {}
	};

	private class IPLCMaster abstract {
	public:
		virtual Platform::String^ device_hostname() = 0;
		virtual Platform::String^ device_description() = 0;

	public:
		virtual Syslog* get_logger() = 0;
		virtual void shake_hands() = 0;
		virtual bool connected() = 0;

	public:
		virtual void send_scheduled_request(long long count, long long interval, long long uptime) = 0;

	public:
		void set_mode(WarGrey::SCADA::PLCMasterMode mode) {
			this->mode = mode;
		}

		WarGrey::SCADA::PLCMasterMode get_mode() {
			return this->mode;
		}

	private:
		WarGrey::SCADA::PLCMasterMode mode = PLCMasterMode::Release;
	};
}
