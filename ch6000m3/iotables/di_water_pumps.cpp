#include "plc.hpp"
#include "iotables/di_water_pumps.hpp"

using namespace WarGrey::SCADA;

void WarGrey::SCADA::DI_water_pump(WaterPumplet* target, const uint8* db4, size_t idx4_p1, const uint8* db205, size_t idx205_p1) {
	target->set_remote_control(DBX(db4, idx4_p1 - 1U));

	if (DBX(db4, idx4_p1 + 1U)) {
		target->set_status(WaterPumpStatus::Running);
	} else if (DBX(db4, idx4_p1 + 2U)) {
		target->set_status(WaterPumpStatus::Alert);
	} else if (DBX(db4, idx4_p1 + 3U)) {
		target->set_status(WaterPumpStatus::Broken);
	//} else if (DBX(db4, idx4 + 5U)) {
	//	target->set_status(WaterPumpStatus::Maintenance);
	} else {
		if (DBX(db205, idx205_p1 - 1U)) {
			target->set_status(WaterPumpStatus::Starting);
		} else if (DBX(db205, idx205_p1 + 0U)) {
			target->set_status(WaterPumpStatus::Stopping);
		} else if (DBX(db205, idx205_p1 + 1U)) {
			target->set_status(WaterPumpStatus::Unstartable);
		} else if (DBX(db205, idx205_p1 + 2U)) {
			target->set_status(WaterPumpStatus::Unstoppable);
		} else if (DBX(db205, idx205_p1 + 3U)) {
			target->set_status(WaterPumpStatus::Ready);
		}
	}

	// the rest are unused or implied
	//target->set_status(DBX(db205, idx205 + 4U), HopperPumpStatus::StartReady);
	//target->set_status(DBX(db205, idx205 + 5U), HopperPumpStatus::StopReady);
}

void WarGrey::SCADA::DI_shift_button(Buttonlet* target, const uint8* db205, unsigned int idx_p1) {
	if (DBX(db205, idx_p1 - 1U)) {
		target->set_status(ButtonStatus::Executing);
	} else if (DBX(db205, idx_p1 + 1U)) {
		target->set_status(ButtonStatus::Ready);
	} else if (DBX(db205, idx_p1 + 3U)) {
		target->set_status(ButtonStatus::Failed);
	} else {
		target->set_status(ButtonStatus::Disabled);
	}
}
