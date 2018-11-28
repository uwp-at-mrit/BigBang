#include "plc.hpp"
#include "iotables/di_hopper_pumps.hpp"

using namespace WarGrey::SCADA;

static void _DI_hopper_pump(HopperPumplet* target, const uint8* db4, size_t idx4_p1, const uint8* db205, size_t idx205_p1, bool on) {
	target->set_remote_control(DI_hopper_pump_remote_control(db4, idx4_p1, on));

	if (on) {
		if (DI_hopper_pump_running(db4, idx4_p1, true)) {
			target->set_status(HopperPumpStatus::Running);
		} else if (DI_hopper_pump_alert(db4, idx4_p1, true)) {
			target->set_status(HopperPumpStatus::Alert);
		} else if (DI_hopper_pump_broken(db4, idx4_p1, true)) {
			target->set_status(HopperPumpStatus::Broken);
		} else if (DI_hopper_pump_repair(db4, idx4_p1, true)) {
			target->set_status(HopperPumpStatus::Maintenance);
		} else if (DBX(db205, idx205_p1 - 1U)) {
			target->set_status(HopperPumpStatus::Starting);
		} else if (DBX(db205, idx205_p1 + 0U)) {
			target->set_status(HopperPumpStatus::Stopping);
		} else if (DI_hopper_pump_ready(db4, idx4_p1, true)) {
			target->set_status(HopperPumpStatus::Ready);
		} else if (DBX(db205, idx205_p1 + 1U)) {
			target->set_status(HopperPumpStatus::Unstartable);
		} else if (DBX(db205, idx205_p1 + 2U)) {
			target->set_status(HopperPumpStatus::Unstoppable);
		} else {
			target->set_status(HopperPumpStatus::Stopped);
		}

		// the rest are unused
		//target->set_status(DBX(db205, idx205 + 4U), HopperPumpStatus::StartReady);
		//target->set_status(DBX(db205, idx205 + 5U), HopperPumpStatus::StopReady);
		//target->set_status(DBX(db205, idx205 + 6U), HopperPumpStatus::Ready);
	} else {
		target->set_status(HopperPumpStatus::Stopped);
	}
}

/*************************************************************************************************/
void WarGrey::SCADA::DI_hopper_pumps(HopperPumplet* t1, HopperPumplet* t2
	, const uint8* db4, size_t idx4_p1 , const uint8* db205, size_t idx205_1_p1, size_t idx205_2_p1) {
	_DI_hopper_pump(t1, db4, idx4_p1, db205, idx205_1_p1, DI_hopper_type(db4, idx4_p1));
	_DI_hopper_pump(t2, db4, idx4_p1, db205, idx205_2_p1, DI_underwater_type(db4, idx4_p1));
}

void WarGrey::SCADA::DI_hopper_pump(HopperPumplet* target, const uint8* db4, size_t idx_p1, const uint8* db205, size_t idx205_p1) {
	_DI_hopper_pump(target, db4, idx_p1, db205, idx205_p1, DI_hopper_type(db4, idx_p1));
}

bool WarGrey::SCADA::DI_hopper_pump_remote_control(const uint8* db4, size_t idx4_p1, bool on) {
	return on && DBX(db4, idx4_p1 + 2U);
}

bool WarGrey::SCADA::DI_hopper_pump_ready(const uint8* db4, size_t idx4_p1, bool on) {
	return on && DBX(db4, idx4_p1 - 1U);
}

bool WarGrey::SCADA::DI_hopper_pump_running(const uint8* db4, size_t idx4_p1, bool on) {
	return on && DBX(db4, idx4_p1 + 5U);
}

bool WarGrey::SCADA::DI_hopper_pump_alert(const uint8* db4, size_t idx4_p1, bool on) {
	return on && DBX(db4, idx4_p1 + 3U);
}

bool WarGrey::SCADA::DI_hopper_pump_broken(const uint8* db4, size_t idx4_p1, bool on) {
	return on && DBX(db4, idx4_p1 + 4U);
}

bool WarGrey::SCADA::DI_hopper_pump_repair(const uint8* db4, size_t idx4_p1, bool on) {
	return on && DBX(db4, idx4_p1 + 6U);
}

/************************************************************************************************/
void WarGrey::SCADA::DI_gland_pump(HydraulicPumplet* target, bool hopper
	, const uint8* db4, size_t idx_p1, const uint8* db205, size_t idx205_p1) {
	target->set_remote_control(DI_gland_pump_remote_control(db4, idx_p1, hopper));

	if (DI_gland_pump_running(db4, idx_p1, hopper)) {
		target->set_status(HydraulicPumpStatus::Running);
	} else if (DI_gland_pump_broken(db4, idx_p1, hopper)) {
		target->set_status(HydraulicPumpStatus::Broken);
	} else if (DI_gland_pump_ready(db4, idx_p1, hopper)) {
		target->set_status(HydraulicPumpStatus::Ready);
	} else {
		target->set_status(DBX(db205, idx205_p1 - 1U), HydraulicPumpStatus::Starting);
		target->set_status(DBX(db205, idx205_p1 + 0U), HydraulicPumpStatus::Stopping);
		//target->set_status(DBX(db205, idx205_p1 + 1), HydraulicPumpStatus::Reset);
		target->set_status(DBX(db205, idx205_p1 + 2U), HydraulicPumpStatus::Unstartable);
		target->set_status(DBX(db205, idx205_p1 + 3U), HydraulicPumpStatus::Unstoppable);

		// the rest 3 are implied or not used
		//target->set_status(DBX(db205, idx205_p1 + 4), HydraulicPumpStatus::StartReady);
		//target->set_status(DBX(db205, idx205_p1 + 5), HydraulicPumpStatus::StopReady);
		//target->set_status(DBX(db205, idx205_p1 + 6), HydraulicPumpStatus::Stopped);
	}
}

bool WarGrey::SCADA::DI_gland_pump_remote_control(const uint8* db4, size_t idx4_p1, bool hopper) {
	size_t idx = (hopper ? idx4_p1 : (idx4_p1 - 1U));

	return DBX(db4, idx);
}

bool WarGrey::SCADA::DI_gland_pump_ready(const uint8* db4, size_t idx4_p1, bool hopper) {
	size_t idx = (hopper ? (idx4_p1 - 1U) : idx4_p1);
	
	return DBX(db4, idx);
}

bool WarGrey::SCADA::DI_gland_pump_broken(const uint8* db4, size_t idx4_p1, bool hopper) {
	return DBX(db4, idx4_p1 + 2U);
}

bool WarGrey::SCADA::DI_gland_pump_running(const uint8* db4, size_t idx4_p1, bool hopper) {
	return DBX(db4, idx4_p1 + 1U);
}

/************************************************************************************************/
void WarGrey::SCADA::DI_hopper_pump_lubricating_unit(HydraulicPumplet* target
	, const uint8* db4, size_t idx4_p1, const uint8* db205, size_t idx205_p1) {
	target->set_remote_control(DBX(db4, idx4_p1 - 1U));

	if (DI_hopper_pump_lubricating_unit_running(db4, idx4_p1)) {
		target->set_status(HydraulicPumpStatus::Running);
	} else if (DBX(db4, idx4_p1 + 1U)) {
		target->set_status(HydraulicPumpStatus::Broken);
	} else {
		target->set_status(HydraulicPumpStatus::Stopped);
	}
}

void WarGrey::SCADA::DI_hopper_pump_gearbox(HydraulicPumplet* target, const uint8* db4, size_t idx4_p1, const uint8* db205, size_t idx205_p1) {
	target->set_remote_control(DBX(db4, idx4_p1 - 1U));

	if (DI_hopper_pump_gearbox_running(db4, idx4_p1)) {
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

bool WarGrey::SCADA::DI_hopper_pump_lubricating_unit_running(const uint8* db4, size_t idx4_p1) {
	return DBX(db4, idx4_p1 + 0U);
}

bool WarGrey::SCADA::DI_hopper_pump_gearbox_running(const uint8* db4, size_t idx4_p1) {
	return DBX(db4, idx4_p1 + 0U);
}

void WarGrey::SCADA::DI_hopper_lubricating_unit_alarm(Alarmlet* target, const uint8* db4, unsigned int idx_p1) {
	target->set_status(DBX(db4, idx_p1 - 1U), AlarmStatus::Alert, AlarmStatus::None);
}
