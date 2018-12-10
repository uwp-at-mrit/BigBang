#include "plc.hpp"
#include "iotables/di_winches.hpp"

using namespace WarGrey::SCADA;

/*************************************************************************************************/
void WarGrey::SCADA::DI_winch(Winchlet* target
	, const uint8* db4, unsigned int feedback_p1, WinchLimits& limits
	, const uint8* db205, WinchDetails& details) {
	bool slack = (limits.slack > 0U) && DBX(db4, limits.slack - 1U);
	bool suction = (limits.suction > 0U) && DBX(db4, limits.suction - 1U);
	bool saddle = DBX(db4, limits.saddle - 1U);

	target->set_remote_control(winch_remote_control(db4, feedback_p1));

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
		unsigned int soft_upper = details.soft_upper - 1U;
		unsigned int soft_lower = details.soft_lower - 1U;
		bool can_windout = (DBX(db205, status + 4U));
		bool can_windup = (DBX(db205, status + 5U));
		bool fast = (details.draghead && DBX(db205, status + 7U));

		if (DBX(db205, status + 0U)) {
			target->set_state(fast, WinchState::FastWindingOut, WinchState::WindingOut);
		} else if (DBX(db205, status + 1U)) {
			target->set_state(fast, WinchState::FastWindingUp, WinchState::WindingUp);
		} else if (DBX(db205, soft_upper + 0U)) {
			target->set_state(WinchState::SoftUpperLimited);
		} else if (DBX(db205, soft_lower + 0U)) {
			target->set_state(WinchState::SoftLowerLimited);
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

bool WarGrey::SCADA::winch_remote_control(const uint8* db4, unsigned int feedback_p1) {
	return DBX(db4, feedback_p1 + 0U);
}
