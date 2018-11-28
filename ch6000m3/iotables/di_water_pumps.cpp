#include "plc.hpp"
#include "iotables/di_water_pumps.hpp"

using namespace WarGrey::SCADA;

void WarGrey::SCADA::DI_water_pump(WaterPumplet* target, const uint8* db4, size_t idx4_p1, const uint8* db205, size_t idx205_p1) {
	target->set_remote_control(DI_water_pump_remote_control(db4, idx4_p1));

	if (DI_water_pump_running(db4, idx4_p1)) {
		target->set_status(WaterPumpState::Running);
	} else if (DI_water_pump_alert(db4, idx4_p1)) {
		target->set_status(WaterPumpState::Alert);
	} else if (DI_water_pump_broken(db4, idx4_p1)) {
		target->set_status(WaterPumpState::Broken);
	//} else if (DI_water_pump_repair(db4, idx4_p1)) {
	//	target->set_status(WaterPumpState::Maintenance);
	} else {
		if (DBX(db205, idx205_p1 - 1U)) {
			target->set_status(WaterPumpState::Starting);
		} else if (DBX(db205, idx205_p1 + 0U)) {
			target->set_status(WaterPumpState::Stopping);
		} else if (DI_water_pump_ready(db4, idx4_p1)) {
			target->set_status(WaterPumpState::Ready);
		} else if (DBX(db205, idx205_p1 + 1U)) {
			target->set_status(WaterPumpState::Unstartable);
		} else if (DBX(db205, idx205_p1 + 2U)) {
			target->set_status(WaterPumpState::Unstoppable);
		} else {
			target->set_status(WaterPumpState::Stopped);
		}
	}

	// the rest are unused or implied
	//target->set_status(DBX(db205, idx205 + 4U), HopperPumpState::StartReady);
	//target->set_status(DBX(db205, idx205 + 5U), HopperPumpState::StopReady);
}

void WarGrey::SCADA::DI_shift_button(Buttonlet* target, const uint8* db205, unsigned int idx_p1) {
	if (DBX(db205, idx_p1 - 1U)) {
		target->set_status(ButtonState::Executing);
	} else if (DBX(db205, idx_p1 + 1U)) {
		target->set_status(ButtonState::Ready);
	} else if (DBX(db205, idx_p1 + 3U)) {
		target->set_status(ButtonState::Failed);
	} else {
		target->set_status(ButtonState::Disabled);
	}
}

/*************************************************************************************************/
bool WarGrey::SCADA::DI_water_pump_remote_control(const uint8* db4, size_t idx4_p1) {
	return DBX(db4, idx4_p1 - 1U);
}

bool WarGrey::SCADA::DI_water_pump_ready(const uint8* db4, size_t idx4_p1) {
	return DBX(db4, idx4_p1 + 0U);
}

bool WarGrey::SCADA::DI_water_pump_running(const uint8* db4, size_t idx4_p1) {
	return DBX(db4, idx4_p1 + 1U);
}

bool WarGrey::SCADA::DI_water_pump_alert(const uint8* db4, size_t idx4_p1) {
	return DBX(db4, idx4_p1 + 2U);
}

bool WarGrey::SCADA::DI_water_pump_broken(const uint8* db4, size_t idx4_p1) {
	return DBX(db4, idx4_p1 + 3U);
}

bool WarGrey::SCADA::DI_water_pump_repair(const uint8* db4, size_t idx4_p1) {
	return DBX(db4, idx4_p1 + 5U);
}

bool WarGrey::SCADA::DI_water_pump_emergence(const uint8* db4, size_t idx4_p1) {
	return DBX(db4, idx4_p1 + 6U);
}
