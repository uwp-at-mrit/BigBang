#pragma once

#include "graphlet/symbol/pump/hopper_pumplet.hpp"
#include "graphlet/symbol/pump/hydraulic_pumplet.hpp"
#include "graphlet/dashboard/alarmlet.hpp"

namespace WarGrey::SCADA {
	// DB4, starts from 1
#define DI_hopper_type(db4, idx_p1) DBX(db4, idx_p1 + 0U)
#define DI_underwater_type(db4, idx_p1) DBX(db4, idx_p1 + 1U)

	static unsigned int ps_hopper_pump_feedback = 1U;
	static unsigned int sb_hopper_pump_feedback = 25U;

	static unsigned int ps_hopper_lubricating_unit_feedback = 481U;
	static unsigned int ps_hopper_lubricating_unit_alarms = 486U;

	static unsigned int ps_hopper_gearbox_master_feedback = 521U;
	static unsigned int ps_hopper_gearbox_spare_feedback = 524U;
	static unsigned int ps_hopper_gearbox_alarms = 527U;

	static unsigned int sb_hopper_lubricating_unit_feedback = 497U;
	static unsigned int sb_hopper_lubricating_unit_alarms = 502U;
	
	static unsigned int sb_hopper_gearbox_master_feedback = 537U;
	static unsigned int sb_hopper_gearbox_spare_feedback = 540U;
	static unsigned int sb_hopper_gearbox_alarms = 543U;

	
	// DB205, starts from 1
	static unsigned int ps_hopper_pump_details = 857U;
	static unsigned int ps_underwater_pump_details = 825U;
	static unsigned int ps_hopper_gearbox_master_details = 1209U;
	static unsigned int ps_hopper_gearbox_spare_details = 1217U;

	static unsigned int sb_hopper_pump_details = 873U;
	static unsigned int sb_underwater_pump_details = 841U;
	static unsigned int sb_hopper_gearbox_master_details = 1225U;
	static unsigned int sb_hopper_gearbox_spare_details = 1233U;

	/************************************************************************************************/
	template<class H>
	void DI_hopper_pump(H* target, const uint8* db4, size_t idx4, const uint8* db205, size_t idx205, bool on) {
		if (on) {
			target->set_remote_control(DBX(db4, idx4 + 3U));

			if (DBX(db4, idx4 + 6U)) {
				target->set_status(HopperPumpStatus::Running);
			} else if (DBX(db4, idx4 + 0U)) {
				target->set_status(HopperPumpStatus::Ready);
			} else if (DBX(db4, idx4 + 4U)) {
				target->set_status(HopperPumpStatus::Alert);
			} else if (DBX(db4, idx4 + 5U)) {
				target->set_status(HopperPumpStatus::Broken);
			} else if (DBX(db4, idx4 + 7U)) {
				target->set_status(HopperPumpStatus::Maintenance);
			} else if (DBX(db205, idx205 + 0U)) {
				target->set_status(HopperPumpStatus::Starting);
			} else if (DBX(db205, idx205 + 1U)) {
				target->set_status(HopperPumpStatus::Stopping);
			} else if (DBX(db205, idx205 + 2U)) {
				target->set_status(HopperPumpStatus::Unstartable);
			} else if (DBX(db205, idx205 + 3U)) {
				target->set_status(HopperPumpStatus::Unstoppable);
			}

			// the rest are unused
			//target->set_status(DBX(db205, idx205 + 4U), HopperPumpStatus::StartReady);
			//target->set_status(DBX(db205, idx205 + 5U), HopperPumpStatus::StopReady);
			//target->set_status(DBX(db205, idx205 + 6U), HopperPumpStatus::Ready);
		}
	}

	template<class H>
	void DI_hopper_pumps(H* t1, H* t2, const uint8* db4, size_t idx4_p1, const uint8* db205, size_t idx205_1_p1, size_t idx205_2_p1) {
		DI_hopper_pump(t1, db4, idx4_p1 - 1U, db205, idx205_1_p1 - 1U, DI_hopper_type(db4, idx4_p1));
		DI_hopper_pump(t2, db4, idx4_p1 - 1U, db205, idx205_2_p1 - 1U, DI_underwater_type(db4, idx4_p1));
	}

	template<class H>
	void DI_hopper_pump(H* target, const uint8* db4, size_t idx_p1, const uint8* db205, size_t idx205_p1) {
		DI_hopper_pump(target, db4, idx_p1 - 1U, db205, idx205_p1 - 1U, DI_hopper_type(db4, idx_p1));
	}

	template<class L>
	void DI_hopper_pump_lubricating_unit(L* target, const uint8* db4, size_t idx4_p1, const uint8* db205, size_t idx205_p1) {
		target->set_remote_control(DBX(db4, idx4_p1 - 1U));

		// the rest are unused
		//target->set_status(DBX(db205, idx205 + 4U), HopperPumpStatus::StartReady);
		//target->set_status(DBX(db205, idx205 + 5U), HopperPumpStatus::StopReady);
	}

	template<class L>
	void DI_hopper_pump_gearbox(L* target, const uint8* db4, size_t idx4_p1, const uint8* db205, size_t idx205_p1) {
		target->set_remote_control(DBX(db4, idx4_p1 - 1U));

		if (DBX(db4, idx4_p1 + 0U)) {
			target->set_status(HydraulicPumpStatus::Running);
		} else if (DBX(db4, idx4_p1 + 1U)) {
			target->set_status(HydraulicPumpStatus::Broken);
		} else if (DBX(db205, idx205_p1 - 1U)) {
			target->set_status(HydraulicPumpStatus::Starting);
		} else if (DBX(db205, idx205_p1 + 0U)) {
			target->set_status(HydraulicPumpStatus::Stopping);
		} else if (DBX(db205, idx205_p1 + 1U)) {
			target->set_status(HydraulicPumpStatus::Unstartable);
		} else if (DBX(db205, idx205_p1 + 2U)) {
			target->set_status(HydraulicPumpStatus::Unstoppable);
		} else if (DBX(db205, idx205_p1 + 3U)) {
			target->set_status(HydraulicPumpStatus::Ready);
		}

		// the rest are unused
		//target->set_status(DBX(db205, idx205 + 4U), HopperPumpStatus::StartReady);
		//target->set_status(DBX(db205, idx205 + 5U), HopperPumpStatus::StopReady);
	}

	template<class A>
	void DI_hopper_lubricating_unit_alarm(A* target, const uint8* db4, unsigned int idx_p1) {
		target->set_status(DBX(db4, idx_p1 - 1U), AlarmStatus::Alert, AlarmStatus::Normal);
	}
}
