#pragma once

#include "graphlet/dashboard/alarmlet.hpp"
#include "graphlet/symbol/heaterlet.hpp"

namespace WarGrey::SCADA {
	// DB4, starts from 1
	static unsigned int filter_01_status = 121U;
	static unsigned int filter_02_status = 127U;
	static unsigned int filter_10_status = 134U;

	static unsigned int tank_heater_feedback = 473U;

	// DB205, starts from 1
	static unsigned int tank_heater_status = 361U;

	void DI_filter_alarm(WarGrey::SCADA::Alarmlet* target, const uint8* db4, unsigned int idx_p1);
	void DI_tank_heater(WarGrey::SCADA::Heaterlet* target, const uint8* db4, size_t idx4_p1, const uint8* db205, size_t idx205_p1);
}
