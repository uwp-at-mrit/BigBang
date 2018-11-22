#include "plc.hpp"
#include "iotables/di_devices.hpp"

using namespace WarGrey::SCADA;

void WarGrey::SCADA::DI_filter_alarm(WarGrey::SCADA::Alarmlet* target, const uint8* db4, unsigned int idx_p1) {
	target->set_status(DBX(db4, idx_p1 - 1U), AlarmStatus::Alert, AlarmStatus::None);
}
