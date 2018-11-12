#pragma once

#include "graphlet/symbol/pump/hopper_pumplet.hpp"

namespace WarGrey::SCADA {
	template<class H>
	void DI_hopper_pump(H* target, const uint8* db4, size_t idx4, const uint8* db205, size_t idx205, bool on) {
		if (on) {
			target->set_remote_control(DBX(db4, idx4 + 3));

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
		bool hopper = DBX(db4, idx4_p1 + 0U);
		bool underw = DBX(db4, idx4_p1 + 1U);

		DI_hopper_pump(t1, db4, idx4_p1 - 1U, db205, idx205_1_p1 - 1U, hopper);
		DI_hopper_pump(t2, db4, idx4_p1 - 1U, db205, idx205_2_p1 - 1U, underw);
	}

	template<class H>
	void DI_hopper_pump(H* target, const uint8* db4, size_t idx_p1, const uint8* db205, size_t idx205_p1) {
		bool hopper = DBX(db4, idx_p1 + 0U);

		DI_hopper_pump(target, db4, idx_p1 - 1U, db205, idx205_p1 - 1U, hopper);
	}
}
