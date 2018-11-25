#include "plc.hpp"
#include "iotables/di_hopper_pumps.hpp"

using namespace WarGrey::SCADA;

static void _DI_hopper_pump(HopperPumplet* target, const uint8* db4, size_t idx4, const uint8* db205, size_t idx205, bool on) {
	if (on) {
		target->set_remote_control(DBX(db4, idx4 + 3U));

		if (DBX(db4, idx4 + 6U)) {
			target->set_status(HopperPumpStatus::Running);
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
		} else if (DBX(db4, idx4 + 0U)) {
			target->set_status(HopperPumpStatus::Ready);
		} else if (DBX(db205, idx205 + 2U)) {
			target->set_status(HopperPumpStatus::Unstartable);
		} else if (DBX(db205, idx205 + 3U)) {
			target->set_status(HopperPumpStatus::Unstoppable);
		} else {
			target->set_status(HopperPumpStatus::Stopped);
		}

		// the rest are unused
		//target->set_status(DBX(db205, idx205 + 4U), HopperPumpStatus::StartReady);
		//target->set_status(DBX(db205, idx205 + 5U), HopperPumpStatus::StopReady);
		//target->set_status(DBX(db205, idx205 + 6U), HopperPumpStatus::Ready);
	}
}

/*************************************************************************************************/
void WarGrey::SCADA::DI_hopper_pumps(HopperPumplet* t1, HopperPumplet* t2
	, const uint8* db4, size_t idx4_p1 , const uint8* db205, size_t idx205_1_p1, size_t idx205_2_p1) {
	_DI_hopper_pump(t1, db4, idx4_p1 - 1U, db205, idx205_1_p1 - 1U, DI_hopper_type(db4, idx4_p1));
	_DI_hopper_pump(t2, db4, idx4_p1 - 1U, db205, idx205_2_p1 - 1U, DI_underwater_type(db4, idx4_p1));
}

void WarGrey::SCADA::DI_hopper_pump(HopperPumplet* target, const uint8* db4, size_t idx_p1, const uint8* db205, size_t idx205_p1) {
	_DI_hopper_pump(target, db4, idx_p1 - 1U, db205, idx205_p1 - 1U, DI_hopper_type(db4, idx_p1));
}

/************************************************************************************************/
void WarGrey::SCADA::DI_hopper_gland_pump(HydraulicPumplet* target, const uint8* db4, size_t idx_p1, const uint8* db205, size_t idx205_p1) {
	target->set_remote_control(DBX(db4, idx_p1 + 0U));

	if (DBX(db4, idx_p1 + 1U)) {
		target->set_status(HydraulicPumpStatus::Running);
	} else if (DBX(db4, idx_p1 + 2U)) {
		target->set_status(HydraulicPumpStatus::Broken);
	} else if (DBX(db4, idx_p1 - 1U)) {
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

void WarGrey::SCADA::DI_underwater_gland_pump(HydraulicPumplet* target, const uint8* db4, size_t idx_p1, const uint8* db205, size_t idx205_p1) {
	target->set_remote_control(DBX(db4, idx_p1 - 1U));

	if (DBX(db4, idx_p1 + 1U)) {
		target->set_status(HydraulicPumpStatus::Running);
	} else if (DBX(db4, idx_p1 + 2U)) {
		target->set_status(HydraulicPumpStatus::Broken);
	} else if (DBX(db4, idx_p1 + 0U)) {
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

/************************************************************************************************/
void WarGrey::SCADA::DI_hopper_pump_lubricating_unit(HydraulicPumplet* target
	, const uint8* db4, size_t idx4_p1, const uint8* db205, size_t idx205_p1) {
	target->set_remote_control(DBX(db4, idx4_p1 - 1U));

	// the rest are unused
	//target->set_status(DBX(db205, idx205 + 4U), HopperPumpStatus::StartReady);
	//target->set_status(DBX(db205, idx205 + 5U), HopperPumpStatus::StopReady);
}

void WarGrey::SCADA::DI_hopper_pump_gearbox(HydraulicPumplet* target, const uint8* db4, size_t idx4_p1, const uint8* db205, size_t idx205_p1) {
	target->set_remote_control(DBX(db4, idx4_p1 - 1U));

	if (DBX(db4, idx4_p1 + 0U)) {
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

void WarGrey::SCADA::DI_hopper_lubricating_unit_alarm(Alarmlet* target, const uint8* db4, unsigned int idx_p1) {
	target->set_status(DBX(db4, idx_p1 - 1U), AlarmStatus::Alert, AlarmStatus::None);
}
