#pragma once

#include "graphlet/dashboard/alarmlet.hpp"

namespace WarGrey::SCADA {
	// DB4, starts from 1
	static unsigned int filter_01_status = 121U;
	static unsigned int filter_02_status = 127U;
	static unsigned int filter_10_status = 134U;

	void DI_filter_alarm(WarGrey::SCADA::Alarmlet* target, const uint8* db4, unsigned int idx_p1);
}
