#pragma once

#include "mrit.hpp"
#include "syslog.hpp"

namespace WarGrey::SCADA {
	private class PLCClient : public WarGrey::SCADA::MRMaster {
	public:
		PLCClient(Syslog* alarm);

	public:
		void send_scheduled_request(long long count, long long interval, long long uptime);

	protected:
		bool fill_signal_preferences(MRSignal type, uint16* data_block, uint16* addr0, uint16* addrn) override;
	};
}
