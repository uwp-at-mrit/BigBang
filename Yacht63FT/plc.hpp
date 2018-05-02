#pragma once

#include "mrit.hpp"
#include "syslog.hpp"

namespace WarGrey::SCADA {
	private class PLCConfirmation : public WarGrey::SCADA::MRConfirmation {
	public:
		void on_all_signals(size_t addr0, size_t addrn, uint8* data, size_t size, WarGrey::SCADA::Syslog* logger) override;
	};

	private class PLCMaster : public WarGrey::SCADA::MRMaster, WarGrey::SCADA::PLCConfirmation {
	public:
		PLCMaster(Syslog* alarm);

	public:
		void send_scheduled_request(long long count, long long interval, long long uptime);
	};
}
