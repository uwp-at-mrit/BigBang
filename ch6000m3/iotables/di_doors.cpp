#include "plc.hpp"
#include "iotables/di_doors.hpp"

using namespace WarGrey::SCADA;

/************************************************************************************************/
void WarGrey::SCADA::DI_hopper_door(IHopperDoorlet* target, const uint8* db205, size_t idx205_p1) {
	if (DBX(db205, idx205_p1 + 6)) {
		target->set_status(DoorState::Disabled);
	} else if (DBX(db205, idx205_p1 - 1)) {
		target->set_status(DoorState::Opening);
	} else if (DBX(db205, idx205_p1 + 0)) {
		target->set_status(DoorState::Closing);
	} else {
		target->set_status(DoorState::Default);
	}
}

void WarGrey::SCADA::DI_hopper_door(IHopperDoorlet* target, const uint8* db4, size_t idx4_p1, const uint8* db205, size_t idx205_p1) {
	DI_hopper_door(target, db205, idx205_p1);
	target->set_status(DBX(db4, idx4_p1 - 1), DoorState::Closed);
}

void WarGrey::SCADA::DI_hopper_doors_auto_lock(Alarmlet* alarmer, const uint8* db205) {
	alarmer->set_status(DBX(db205, 1087U - 1U), AlarmState::Notice, AlarmState::None);
}

void WarGrey::SCADA::DI_hopper_doors_locked(Alarmlet* alarmer, const uint8* db205) {
	alarmer->set_status(DBX(db205, 1086U - 1U), AlarmState::Notice, AlarmState::None);
}

void WarGrey::SCADA::DI_hopper_doors_checks_button(Buttonlet* button, BottomDoorCommand cmd, const uint8* db205) {
	unsigned int idx_p1 = 0U;

	switch (cmd) {
	case BottomDoorCommand::OpenDoorCheck:  idx_p1 = 1897U; break;
	case BottomDoorCommand::CloseDoorCheck: idx_p1 = 1899U; break;
	}

	if (idx_p1 > 0) {
		if (DBX(db205, idx_p1 + 0U)) {
			button->set_status(ButtonState::Executing);
		} else if (DBX(db205, idx_p1 - 1U)) {
			button->set_status(ButtonState::Ready);
		} else {
			button->set_status(ButtonState::Disabled);
		}
	}
}
