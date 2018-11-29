#include "plc.hpp"
#include "iotables/di_dredges.hpp"

using namespace WarGrey::SCADA;

template<class G>
static void _DI_gantry(G* target, const uint8* db4, unsigned int idx4, const uint8* db205, unsigned int idx205) {
	if (DBX(db4, idx4 + 0U)) {
		target->set_state(GantryState::WindedOut);
	} else if (DBX(db4, idx4 + 1U)) {
		target->set_state(GantryState::WindedUp);
	} else if (DBX(db205, idx205 + 0U)) {
		target->set_state(GantryState::WindingOut);
	} else if (DBX(db205, idx205 + 1U)) {
		target->set_state(GantryState::WindingUp);
	} else {
		target->set_state(GantryState::Default);
	}
}

/*************************************************************************************************/
void WarGrey::SCADA::DI_winch(Winchlet* target, const uint8* db4, WinchLimits& limits, const uint8* db205, WinchDetails& details) {
	bool slack = (limits.slack > 0U) && DBX(db4, limits.slack - 1U);
	bool suction = (limits.suction > 0U) && DBX(db4, limits.suction - 1U);
	bool saddle = DBX(db4, limits.saddle - 1U);

	if (DBX(db4, limits.upper - 1U)) {
		target->set_state(WinchState::UpperLimited);
	} else if (slack || suction || saddle) {
		if (suction) {
			target->set_state(slack, WinchState::SuctionSlack, WinchState::SuctionLimited);
		} else if (saddle) {
			target->set_state(slack, WinchState::SaddleSlack, WinchState::SaddleLimited);
		} else {
			target->set_state(WinchState::Slack);
		}
	} else {
		unsigned int status = details.status - 1U;
		unsigned int sensor = details.sensor - 1U;
		bool can_windout = (DBX(db205, status + 4U));
		bool can_windup = (DBX(db205, status + 5U));
		bool fast = (details.draghead && DBX(db205, status + 7U));

		if (DBX(db205, status + 0U)) {
			target->set_state(fast, WinchState::FastWindingOut, WinchState::WindingOut);
		} else if (DBX(db205, status + 1U)) {
			target->set_state(fast, WinchState::FastWindingUp, WinchState::WindingUp);
		} else if (DBX(db205, sensor + 0U)) {
			target->set_state(WinchState::SensorUpperLimited);
		} else if (DBX(db205, sensor + 1U)) {
			target->set_state(WinchState::SensorLowerLimited);
		} else if (can_windout && can_windup) {
			target->set_state(fast, WinchState::FastWindReady, WinchState::WindReady);
		} else if (can_windout) {
			target->set_state(fast, WinchState::FastWindOutReady, WinchState::WindOutReady);
		} else if (can_windup) {
			target->set_state(fast, WinchState::FastWindUpReady, WinchState::WindUpReady);
		}

		// the rest are unused;
		//  target->set_state(DBX(db205, status + 2U), WinchState::Unlettable);
		//  target->set_state(DBX(db205, status + 3U), WinchState::Unpullable);
	}
}

void WarGrey::SCADA::DI_winch_override(Buttonlet* target, const uint8* db205, WinchDetails& details) {
	target->set_state(DBX(db205, details.override - 1U), ButtonState::Executing, ButtonState::Default);
}

void WarGrey::SCADA::DI_gantry(Gantrylet* target, const uint8* db4, unsigned int idx4_p1, const uint8* db205, unsigned int idx205_p1) {
	_DI_gantry(target, db4, idx4_p1 - 1U, db205, idx205_p1 - 1U);
}

void WarGrey::SCADA::DI_gantry(GantrySymbollet* target, const uint8* db4, unsigned int idx4_p1, const uint8* db205, unsigned int idx205_p1) {
	_DI_gantry(target, db4, idx4_p1 - 1U, db205, idx205_p1 - 1U);
}

void WarGrey::SCADA::DI_suction_buttons(Buttonlet* intarget, Buttonlet* detarget, const uint8* db205, unsigned int idx_p1) {
	if (DBX(db205, idx_p1 - 1U)) {
		intarget->set_state(ButtonState::Executing);
	} else if (DBX(db205, idx_p1 + 1U)) {
		intarget->set_state(ButtonState::Failed);
	} else if (DBX(db205, idx_p1 + 3U)) {
		intarget->set_state(ButtonState::Ready);
	} else {
		intarget->set_state(ButtonState::Default);
	}

	if (DBX(db205, idx_p1 + 0U)) {
		detarget->set_state(ButtonState::Executing);
	} else if (DBX(db205, idx_p1 + 2U)) {
		detarget->set_state(ButtonState::Failed);
	} else if (DBX(db205, idx_p1 + 4U)) {
		detarget->set_state(ButtonState::Ready);
	} else {
		detarget->set_state(ButtonState::Default);
	}
}

void WarGrey::SCADA::DI_ctension_button(Buttonlet* target, const uint8* db205, unsigned int idx_p1) {
	target->set_state(DBX(db205, idx_p1 - 1U), ButtonState::Executing, ButtonState::Default);
}

bool WarGrey::SCADA::DI_long_sb_drag(const uint8* db205) {
	return DBX(db205, 1417U - 1U);
}
