#include "plc.hpp"
#include "iotables/di_devices.hpp"

using namespace WarGrey::SCADA;

void WarGrey::SCADA::DI_filter_alarm(WarGrey::SCADA::Alarmlet* target, const uint8* db4, unsigned int idx_p1) {
	target->set_status(DBX(db4, idx_p1 - 1U), AlarmState::Alert, AlarmState::None);
}

void WarGrey::SCADA::DI_tank_heater(Heaterlet* target, const uint8* db4, size_t idx4_p1, const uint8* db205, size_t idx205_p1) {
	target->set_remote_control(DBX(db4, idx4_p1 - 1U));
	target->set_auto_mode(DBX(db205, idx205_p1 + 6U));

	if (DBX(db4, idx4_p1 + 1U)) {
		target->set_status(HeaterState::Broken);
	} else if (DBX(db4, idx4_p1 + 0U)) {
		target->set_status(HeaterState::Running);	
	} else if (DBX(db205, idx205_p1 - 1U)) {
		target->set_status(HeaterState::Starting);
	} else if (DBX(db205, idx205_p1 + 0U)) {
		target->set_status(HeaterState::Stopping);
	} else if (DBX(db205, idx205_p1 + 3U)) {
		target->set_status(HeaterState::Ready);
	} else if (DBX(db205, idx205_p1 + 1U)) {
		target->set_status(HeaterState::Unstartable);
	} else if (DBX(db205, idx205_p1 + 2U)) {
		target->set_status(HeaterState::Unstoppable);
	} else {
		target->set_status(HeaterState::Stopped);
	}
}
