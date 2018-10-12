#pragma once

#include "graphlet/symbol/pump/hydraulic_pumplet.hpp"

namespace WarGrey::SCADA {
	template<class H>
	void DI_hydraulic_pump(H* target, const uint8* db4, size_t idx4_p1, const uint8* db205, size_t idx205_p1) {
		target->set_remote_control(DBX(db4, idx4_p1 - 1));

		target->set_status(DBX(db4, idx4_p1 + 0), HydraulicPumpStatus::Running);
		target->set_status(DBX(db4, idx4_p1 + 1), HydraulicPumpStatus::Broken);

		target->set_status(DBX(db205, idx205_p1 - 1), HydraulicPumpStatus::Starting);
		target->set_status(DBX(db205, idx205_p1 + 0), HydraulicPumpStatus::Stopping);
		target->set_status(DBX(db205, idx205_p1 + 1), HydraulicPumpStatus::Unstartable);
		target->set_status(DBX(db205, idx205_p1 + 2), HydraulicPumpStatus::Unstoppable);
		target->set_status(DBX(db205, idx205_p1 + 3), HydraulicPumpStatus::StartReady);
		target->set_status(DBX(db205, idx205_p1 + 4), HydraulicPumpStatus::StopReady);
		target->set_status(DBX(db205, idx205_p1 + 5), HydraulicPumpStatus::Stopped);
		target->set_status(DBX(db205, idx205_p1 + 6), HydraulicPumpStatus::Ready);
	}

	template<class F>
	void DI_flushing_pump(F* target, const uint8* db4, size_t idx4_p1, const uint8* db205, size_t idx205_p1) {
		target->set_remote_control(DBX(db4, idx4_p1 - 1));

		target->set_status(DBX(db4, idx4_p1 + 0), HydraulicPumpStatus::Running);
		target->set_status(DBX(db4, idx4_p1 + 1), HydraulicPumpStatus::Broken);

		target->set_status(DBX(db205, idx205_p1 - 1), HydraulicPumpStatus::Starting);
		target->set_status(DBX(db205, idx205_p1 + 0), HydraulicPumpStatus::Stopping);
		target->set_status(DBX(db205, idx205_p1 + 1), HydraulicPumpStatus::Unstartable);
		target->set_status(DBX(db205, idx205_p1 + 2), HydraulicPumpStatus::Unstoppable);
		target->set_status(DBX(db205, idx205_p1 + 3), HydraulicPumpStatus::StartReady);
		target->set_status(DBX(db205, idx205_p1 + 4), HydraulicPumpStatus::StopReady);
		target->set_status(DBX(db205, idx205_p1 + 5), HydraulicPumpStatus::Stopped);
		target->set_status(DBX(db205, idx205_p1 + 6), HydraulicPumpStatus::Ready);
	}

	template<class S>
	void DI_sealed_water_pump(S* target, bool underwater, const uint8* db4, size_t idx_p1, const uint8* db205, size_t idx205_p1) {
		if (underwater) {
			target->set_remote_control(DBX(db4, idx_p1 - 1));
			target->set_status(DBX(db4, idx_p1 + 0), HydraulicPumpStatus::Ready);
		} else {
			target->set_remote_control(DBX(db4, idx_p1 + 0));
			target->set_status(DBX(db4, idx_p1 - 1), HydraulicPumpStatus::Ready);
		}

		target->set_status(DBX(db4, idx_p1 + 1), HydraulicPumpStatus::Running);
		target->set_status(DBX(db4, idx_p1 + 2), HydraulicPumpStatus::Broken);

		target->set_status(DBX(db205, idx205_p1 - 1), HydraulicPumpStatus::Starting);
		target->set_status(DBX(db205, idx205_p1 + 0), HydraulicPumpStatus::Stopping);
		target->set_status(DBX(db205, idx205_p1 + 1), HydraulicPumpStatus::Unstartable);
		target->set_status(DBX(db205, idx205_p1 + 2), HydraulicPumpStatus::Unstoppable);
		target->set_status(DBX(db205, idx205_p1 + 3), HydraulicPumpStatus::StartReady);
		target->set_status(DBX(db205, idx205_p1 + 4), HydraulicPumpStatus::StopReady);
		target->set_status(DBX(db205, idx205_p1 + 5), HydraulicPumpStatus::Stopped);
		target->set_status(DBX(db205, idx205_p1 + 6), HydraulicPumpStatus::Ready);
	}
}
