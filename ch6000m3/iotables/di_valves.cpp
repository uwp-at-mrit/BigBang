#include "plc.hpp"
#include "iotables/di_valves.hpp"

using namespace WarGrey::SCADA;

void WarGrey::SCADA::DI_manual_valve(ManualValvelet* target, const uint8* db4, size_t idx_p1) {
	// OpenReady or StopReady
	target->set_status(DBX(db4, idx_p1 - 1), ManualValveStatus::Open, ManualValveStatus::OpenReady);
}

void WarGrey::SCADA::DI_gate_valve(GateValvelet* target, const uint8* db4, size_t idx4_p1, const uint8* db205, size_t idx205_p1) {
	if (DBX(db205, idx205_p1 + 5U)) {
		target->set_status(GateValveStatus::VirtualOpen);
	} else if (DBX(db205, idx205_p1 + 6U)) {
		target->set_status(GateValveStatus::VirtualClose);
	} else if (DBX(db4, idx4_p1 - 1U)) {
		target->set_status(GateValveStatus::Open);
	} else if (DBX(db4, idx4_p1 + 0U)) {
		target->set_status(GateValveStatus::Closed);
	} else if (DBX(db205, idx205_p1 - 1U)) {
		target->set_status(GateValveStatus::Opening);
	} else if (DBX(db205, idx205_p1 + 0U)) {
		target->set_status(GateValveStatus::Closing);
	} else if (DBX(db205, idx205_p1 + 3U)) {
		target->set_status(GateValveStatus::OpenReady);
	} else if (DBX(db205, idx205_p1 + 4U)) {
		target->set_status(GateValveStatus::CloseReady);
	} else if (DBX(db205, idx205_p1 + 1U)) {
		target->set_status(GateValveStatus::Unopenable);
	} else if (DBX(db205, idx205_p1 + 2U)) {
		target->set_status(GateValveStatus::Unclosable);
	}
}

void WarGrey::SCADA::DI_motor_valve(MotorValvelet* target, const uint8* db4, size_t idx4_p1, const uint8* db205, size_t idx205_p1) {
	if (DBX(db205, idx205_p1 + 5U)) {
		target->set_status(TValveStatus::VirtualOpen);
	} else if (DBX(db205, idx205_p1 + 6U)) {
		target->set_status(TValveStatus::VirtualClose);
	} else if (DBX(db4, idx4_p1 - 1U)) {
		target->set_status(TValveStatus::Open);
	} else if (DBX(db4, idx4_p1 + 0U)) {
		target->set_status(TValveStatus::Closed);
	} else if (DBX(db205, idx205_p1 - 1U)) {
		target->set_status(TValveStatus::Opening);
	} else if (DBX(db205, idx205_p1 + 0U)) {
		target->set_status(TValveStatus::Closing);
	} else if (DBX(db205, idx205_p1 + 3U)) {
		target->set_status(TValveStatus::OpenReady);
	} else if (DBX(db205, idx205_p1 + 4U)) {
		target->set_status(TValveStatus::CloseReady);
	} else if (DBX(db205, idx205_p1 + 1U)) {
		target->set_status(TValveStatus::Unopenable);
	} else if (DBX(db205, idx205_p1 + 2U)) {
		target->set_status(TValveStatus::Unclosable);
	}
}
