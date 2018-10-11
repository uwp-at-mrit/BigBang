#pragma once

#include "mrit.hpp"
#include "syslog.hpp"

namespace WarGrey::SCADA {
	float AI_ref(const uint8* db4, size_t idx, float scale = 1.0F);
	bool DI_ref(const uint8* db28, size_t idx);
	unsigned int DI_ref(const uint8* db28, size_t idx0, size_t idxn);

	private class PLCConfirmation : public WarGrey::SCADA::MRConfirmation {
	public:
		void on_all_signals(size_t addr0, size_t addrn, uint8* data, size_t size, WarGrey::SCADA::Syslog* logger) override;

	public:
		virtual void on_digital_input_data(uint8* data, size_t size, WarGrey::SCADA::Syslog* logger) {}
		virtual void on_analog_input(uint8* data, size_t size, WarGrey::SCADA::Syslog* logger) {}
	};

	private class PLCMaster : public WarGrey::SCADA::MRMaster {
	public:
		PLCMaster(Syslog* alarm);
	};
}
