#pragma once

#include "mrit.hpp"
#include "syslog.hpp"

namespace WarGrey::SCADA {
	private class PLCMaster : public WarGrey::SCADA::MRMaster {
	public:
		PLCMaster(Syslog* alarm);

	public:
		void send_scheduled_request(long long count, long long interval, long long uptime);

	protected:
		bool fill_signal_preferences(uint16 type, uint16* count, uint16* addr0, uint16* addrn) override;
	};
}
