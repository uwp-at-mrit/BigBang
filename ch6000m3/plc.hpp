#pragma once

#include "mrit.hpp"
#include "syslog.hpp"

namespace WarGrey::SCADA {
	private class PLCMaster : public WarGrey::SCADA::MRMaster, WarGrey::SCADA::MRConfirmation {
	public:
		PLCMaster(Syslog* alarm);

	public:
		void send_scheduled_request(long long count, long long interval, long long uptime);

	public:
		void on_realtime_data(float* db2, uint16 count, Syslog* logger) override;

	protected:
		bool fill_signal_preferences(uint16 type, uint16* count, uint16* addr0, uint16* addrn) override;

	private:
		float tidemark;
	};
}
