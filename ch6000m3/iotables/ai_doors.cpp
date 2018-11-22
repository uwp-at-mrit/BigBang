#include "iotables/ai_doors.hpp"
#include "graphlet/symbol/door/hopper_doorlet.hpp"

using namespace WarGrey::SCADA;

void WarGrey::SCADA::AI_hopper_door(IHopperDoorlet* target, float progress, float open_threshold, float closed_threshold) {
	DoorStatus s = target->get_status();

	if (s == DoorStatus::Opening) {
		if (progress >= open_threshold) {
			target->set_status(DoorStatus::Open);
		}
	}

	if (s == DoorStatus::Closing) {
		if (progress <= closed_threshold) {
			target->set_status(DoorStatus::Closed);
		}
	}
}
