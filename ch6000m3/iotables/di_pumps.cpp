#include "plc.hpp"
#include "iotables/di_pumps.hpp"

using namespace WarGrey::SCADA;

void WarGrey::SCADA::DI_hydraulic_pump_dimension(Dimensionlet* target, const uint8* db4, size_t idx_p1) {
	target->set_status(DBX(db4, idx_p1) ? DimensionStatus::Highlight : DimensionStatus::Normal);
}

void WarGrey::SCADA::DI_hydraulic_pump(HydraulicPumplet* target, const uint8* db4, size_t idx4_p1, const uint8* db205, size_t idx205_p1) {
	target->set_remote_control(DI_hydraulic_pump_remote_control(db4, idx4_p1));

	if (DI_hydraulic_pump_broken(db4, idx4_p1)) {
		target->set_status(HydraulicPumpStatus::Broken);
	} else {
		if (DI_hydraulic_pump_running(db4, idx4_p1)) {
			// equivalent
			target->set_status(DBX(db205, idx205_p1 + 4U), HydraulicPumpStatus::StopReady);
			target->set_status(HydraulicPumpStatus::Running);
			// use HydraulicPumpStatus::Ready instead of HydraulicPumpStatus::StartReady.	
		} else if (DBX(db205, idx205_p1 - 1U)) {
			target->set_status(HydraulicPumpStatus::Starting);
		} else if (DBX(db205, idx205_p1 + 0U)) {
			target->set_status(HydraulicPumpStatus::Stopping);
		} else if (DI_hydraulic_pump_ready(db205, idx205_p1)) {
			target->set_status(HydraulicPumpStatus::Ready);
		} else if (DBX(db205, idx205_p1 + 1U)) {
			target->set_status(HydraulicPumpStatus::Unstartable);
		} else if (DBX(db205, idx205_p1 + 2U)) {
			target->set_status(HydraulicPumpStatus::Unstoppable);
		} else {
			target->set_status(HydraulicPumpStatus::Stopped);
		}
		
		// the rest two are not used
		// target->set_status(DBX(db205, idx205_p1 + 6), HydraulicPumpStatus::Ready);
	}
}

bool WarGrey::SCADA::DI_hydraulic_pump_remote_control(const uint8* db4, size_t idx4_p1) {
	return DBX(db4, idx4_p1 - 1U);
}

bool WarGrey::SCADA::DI_hydraulic_pump_running(const uint8* db4, size_t idx4_p1) {
	return DBX(db4, idx4_p1 + 0U);
}

bool WarGrey::SCADA::DI_hydraulic_pump_broken(const uint8* db4, size_t idx4_p1) {
	return DBX(db4, idx4_p1 + 1U);
}

bool WarGrey::SCADA::DI_hydraulic_pump_ready(const uint8* db205, size_t idx205_p1) {
	return DBX(db205, idx205_p1 + 3U);
}

/************************************************************************************************/
void WarGrey::SCADA::DI_gate_flushing_pump(HydraulicPumplet* target, const uint8* db4, size_t idx4_p1, const uint8* db205, size_t idx205_p1) {
	DI_hydraulic_pump(target, db4, idx4_p1, db205, idx205_p1);
}

bool WarGrey::SCADA::DI_gate_flushing_pump_remote_control(const uint8* db4, size_t idx4_p1) {
	return DI_hydraulic_pump_remote_control(db4, idx4_p1);
}

bool WarGrey::SCADA::DI_gate_flushing_pump_running(const uint8* db4, size_t idx4_p1) {
	return DI_hydraulic_pump_running(db4, idx4_p1);
}

bool WarGrey::SCADA::DI_gate_flushing_pump_broken(const uint8* db4, size_t idx4_p1) {
	return DI_hydraulic_pump_broken(db4, idx4_p1);
}

bool WarGrey::SCADA::DI_gate_flushing_pump_ready(const uint8* db205, size_t idx205_p1) {
	return DI_hydraulic_pump_ready(db205, idx205_p1);
}
