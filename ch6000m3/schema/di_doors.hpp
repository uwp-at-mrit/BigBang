#pragma once

#include "graphlet/symbol/door/hopper_doorlet.hpp"

namespace WarGrey::SCADA {
	// DB4, starts from 1
	static unsigned int bottom_door_PS1_closed = 329U;
	static unsigned int bottom_door_PS2_closed = 330U;
	static unsigned int bottom_door_PS3_closed = 331U;
	static unsigned int bottom_door_PS4_closed = 369U;
	static unsigned int bottom_door_PS5_closed = 370U;
	static unsigned int bottom_door_PS6_closed = 371U;
	static unsigned int bottom_door_PS7_closed = 372U;

	static unsigned int bottom_door_SB1_closed = 345U;
	static unsigned int bottom_door_SB2_closed = 346U;
	static unsigned int bottom_door_SB3_closed = 347U;
	static unsigned int bottom_door_SB4_closed = 401U;
	static unsigned int bottom_door_SB5_closed = 402U;
	static unsigned int bottom_door_SB6_closed = 403U;
	static unsigned int bottom_door_SB7_closed = 404U;

	// missing
	static unsigned int upper_door_PS1_closed = 329U;
	static unsigned int upper_door_PS2_closed = 330U;
	static unsigned int upper_door_PS3_closed = 331U;
	static unsigned int upper_door_PS4_closed = 369U;
	static unsigned int upper_door_PS5_closed = 370U;
	static unsigned int upper_door_PS6_closed = 371U;
	static unsigned int upper_door_PS7_closed = 372U;

	static unsigned int upper_door_SB1_closed = 345U;
	static unsigned int upper_door_SB2_closed = 346U;
	static unsigned int upper_door_SB3_closed = 347U;
	static unsigned int upper_door_SB4_closed = 401U;
	static unsigned int upper_door_SB5_closed = 402U;
	static unsigned int upper_door_SB6_closed = 403U;
	static unsigned int upper_door_SB7_closed = 404U;

	// DB205, starts from 1
	static unsigned int bottom_door_PS1_status = 889U;
	static unsigned int bottom_door_PS2_status = 905U;
	static unsigned int bottom_door_PS3_status = 921U;
	static unsigned int bottom_door_PS4_status = 937U;
	static unsigned int bottom_door_PS5_status = 953U;
	static unsigned int bottom_door_PS6_status = 969U;
	static unsigned int bottom_door_PS7_status = 985U;

	static unsigned int bottom_door_SB1_status = 897U;
	static unsigned int bottom_door_SB2_status = 913U;
	static unsigned int bottom_door_SB3_status = 929U;
	static unsigned int bottom_door_SB4_status = 945U;
	static unsigned int bottom_door_SB5_status = 961U;
	static unsigned int bottom_door_SB6_status = 977U;
	static unsigned int bottom_door_SB7_status = 993U;

	static unsigned int upper_door_PS1_status = 1089U;
	static unsigned int upper_door_PS2_status = 1105U;
	static unsigned int upper_door_PS3_status = 1121U;
	static unsigned int upper_door_PS4_status = 1137U;
	static unsigned int upper_door_PS5_status = 1153U;
	static unsigned int upper_door_PS6_status = 1169U;
	static unsigned int upper_door_PS7_status = 1185U;

	static unsigned int upper_door_SB1_status = 1097U;
	static unsigned int upper_door_SB2_status = 1113U;
	static unsigned int upper_door_SB3_status = 1129U;
	static unsigned int upper_door_SB4_status = 1145U;
	static unsigned int upper_door_SB5_status = 1161U;
	static unsigned int upper_door_SB6_status = 1177U;
	static unsigned int upper_door_SB7_status = 1193U;

	/************************************************************************************************/
	template<class D>
	void DI_hopper_door(D* target, const uint8* db4, size_t idx4_p1, const uint8* db205, size_t idx205_p1) {
		target->set_status(DBX(db4, idx4_p1 - 1), DoorStatus::Closed);
		
		target->set_status(DBX(db205, idx205_p1 - 1), DoorStatus::Opening);
		target->set_status(DBX(db205, idx205_p1 + 0), DoorStatus::Closing);
		target->set_status(DBX(db205, idx205_p1 + 6), DoorStatus::Disabled);
	}
}
