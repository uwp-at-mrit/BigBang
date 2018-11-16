#pragma once

#include "graphlet/symbol/pump/water_pumplet.hpp"

namespace WarGrey::SCADA {
	// DB4, starts from 1
	static unsigned int ps_water_pump_feedback = 17U;
	static unsigned int sb_water_pump_feedback = 41U;

	// DB205, starts from 1
	static unsigned int ps_water_pump_details = 1001U;
	static unsigned int sb_water_pump_details = 1017U;

	/************************************************************************************************/
	template<class H>
	void DI_water_pump(H* target, const uint8* db4, size_t idx4_p1, const uint8* db205, size_t idx205_p1) {
		target->set_remote_control(DBX(db4, idx4_p1 - 1U));

		if (DBX(db4, idx4_p1 + 1U)) {
			target->set_status(WaterPumpStatus::Running);
		} else if (DBX(db4, idx4_p1 + 0U)) {
			target->set_status(WaterPumpStatus::Ready);
		} else if (DBX(db4, idx4_p1 + 2U)) {
			target->set_status(WaterPumpStatus::Alert);
		} else if (DBX(db4, idx4_p1 + 3U)) {
			target->set_status(WaterPumpStatus::Broken);
		//} else if (DBX(db4, idx4 + 5U)) {
		//	target->set_status(WaterPumpStatus::Maintenance);
		} else if (DBX(db205, idx205_p1 - 1U)) {
			target->set_status(WaterPumpStatus::Starting);
		} else if (DBX(db205, idx205_p1 + 0U)) {
			target->set_status(WaterPumpStatus::Stopping);
		} else if (DBX(db205, idx205_p1 + 1U)) {
			target->set_status(WaterPumpStatus::Unstartable);
		} else if (DBX(db205, idx205_p1 + 2U)) {
			target->set_status(WaterPumpStatus::Unstoppable);
		} else if (DBX(db205, idx205_p1 + 3U)) {
			target->set_status(WaterPumpStatus::StartReady);
		}

		// the rest are unused
		//target->set_status(DBX(db205, idx205 + 4U), HopperPumpStatus::StartReady);
		//target->set_status(DBX(db205, idx205 + 5U), HopperPumpStatus::StopReady);
		//target->set_status(DBX(db205, idx205 + 6U), HopperPumpStatus::Ready);
	}
}
