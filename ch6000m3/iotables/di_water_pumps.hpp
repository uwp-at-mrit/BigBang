#pragma once

#include "graphlet/symbol/pump/water_pumplet.hpp"
#include "graphlet/buttonlet.hpp"

namespace WarGrey::SCADA {
	// DB4, starts from 1
	static unsigned int ps_water_pump_feedback = 17U;
	static unsigned int sb_water_pump_feedback = 41U;

	// DB205, starts from 1
	static unsigned int ps_water_pump_details = 1001U;
	static unsigned int sb_water_pump_details = 1017U;

	static unsigned int left_shifting_details = 1249U;
	static unsigned int right_shifting_details = 1250U;

	/************************************************************************************************/
	void DI_water_pump(WarGrey::SCADA::WaterPumplet* target, const uint8* db4, size_t idx4_p1, const uint8* db205, size_t idx205_p1);

	void DI_shift_button(WarGrey::SCADA::Buttonlet* target, const uint8* db205, unsigned int idx_p1);
}
