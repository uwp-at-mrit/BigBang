#include "plc.hpp"
#include "iotables/di_devices.hpp"

using namespace WarGrey::SCADA;

void WarGrey::SCADA::DI_filter_alarm(WarGrey::SCADA::Alarmlet* target, const uint8* db4, unsigned int idx_p1) {
	target->set_status(DBX(db4, idx_p1 - 1U), AlarmStatus::Alert, AlarmStatus::None);
}

void WarGrey::SCADA::DI_tank_heater(Heaterlet* target, const uint8* db4, size_t idx4_p1, const uint8* db205, size_t idx205_p1) {
	target->set_remote_control(DBX(db4, idx4_p1 - 1U));

	if (DBX(db4, idx4_p1 + 1U)) {
		target->set_status(HeaterStatus::Broken);
	} else if (DBX(db4, idx4_p1 + 0U)) {
		target->set_status(HeaterStatus::Running);	
	} else if (DBX(db205, idx205_p1 - 1U)) {
		target->set_status(HeaterStatus::Starting);
	} else if (DBX(db205, idx205_p1 + 0U)) {
		target->set_status(HeaterStatus::Stopping);
	} else if (DBX(db205, idx205_p1 + 3U)) {
		target->set_status(HeaterStatus::Ready);
	} else if (DBX(db205, idx205_p1 + 1U)) {
		target->set_status(HeaterStatus::Unstartable);
	} else if (DBX(db205, idx205_p1 + 2U)) {
		target->set_status(HeaterStatus::Unstoppable);
	} else if (DBX(db205, idx205_p1 + 6U)) {
		target->set_status(HeaterStatus::Auto);
	} else {
		target->set_status(HeaterStatus::Stopped);
	}
}
