#pragma once

#include "graphlet/symbol/pump/hopper_pumplet.hpp"

namespace WarGrey::SCADA {
	template<class H>
	void DI_hopper_pump(H* target, const uint8* db4, size_t idx4, const uint8* db205, size_t idx205, bool on) {
		if (on) {
			target->set_remote_control(DBX(db4, idx4 + 3));

			target->set_status(DBX(db4, idx4 + 0), HopperPumpStatus::Ready);
			target->set_status(DBX(db4, idx4 + 4), HopperPumpStatus::Alert);
			target->set_status(DBX(db4, idx4 + 5), HopperPumpStatus::Broken);
			target->set_status(DBX(db4, idx4 + 6), HopperPumpStatus::Running);
			target->set_status(DBX(db4, idx4 + 7), HopperPumpStatus::Maintenance);

			target->set_status(DBX(db205, idx205 + 0), HopperPumpStatus::Starting);
			target->set_status(DBX(db205, idx205 + 1), HopperPumpStatus::Stopping);
			target->set_status(DBX(db205, idx205 + 2), HopperPumpStatus::Unstartable);
			target->set_status(DBX(db205, idx205 + 3), HopperPumpStatus::Unstoppable);
			target->set_status(DBX(db205, idx205 + 4), HopperPumpStatus::StartReady);
			target->set_status(DBX(db205, idx205 + 5), HopperPumpStatus::StopReady);
			target->set_status(DBX(db205, idx205 + 6), HopperPumpStatus::Ready);
		}
	}

	template<class H>
	void DI_hopper_pumps(H* t1, H* t2, const uint8* db4, size_t idx4_p1, const uint8* db205, size_t idx205_1_p1, size_t idx205_2_p1) {
		bool hopper = DBX(db4, idx4_p1 + 0);
		bool underw = DBX(db4, idx4_p1 + 1);

		DI_hopper_pump(t1, db4, idx4_p1 - 1, db205, idx205_1_p1 - 1, hopper);
		DI_hopper_pump(t2, db4, idx4_p1 - 1, db205, idx205_2_p1 - 1, underw);
	}

	template<class H>
	void DI_hopper_pump(H* target, const uint8* db4, size_t idx_p1, const uint8* db205, size_t idx205_p1) {
		bool hopper = DBX(db4, idx_p1 + 0);

		DI_hopper_pump(target, db4, idx_p1 - 1, db205, idx205_p1 - 1, hopper);
	}
}
