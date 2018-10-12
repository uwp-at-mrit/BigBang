#pragma once

#include "graphlet/symbol/door/hopper_doorlet.hpp"

namespace WarGrey::SCADA {
	template<class D>
	void DI_hopper_door(D* target, const uint8* db4, size_t idx4_p1, const uint8* db205, size_t idx205_p1) {
		target->set_status(DBX(db4, idx4_p1 - 1), DoorStatus::Closed);
		
		target->set_status(DBX(db205, idx205_p1 - 1), DoorStatus::Opening);
		target->set_status(DBX(db205, idx205_p1 + 0), DoorStatus::Closing);
		target->set_status(DBX(db205, idx205_p1 + 6), DoorStatus::Disabled);
	}
}
