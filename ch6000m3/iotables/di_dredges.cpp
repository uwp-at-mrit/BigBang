#include "plc.hpp"
#include "iotables/di_dredges.hpp"

using namespace WarGrey::SCADA;

template<class G>
static void _DI_gantry(G* target, const uint8* db4, unsigned int idx4, const uint8* db205, unsigned int idx205) {
	if (DBX(db4, idx4 + 0U)) {
		target->set_status(GantryStatus::WindedOut);
	} else if (DBX(db4, idx4 + 1U)) {
		target->set_status(GantryStatus::WindedUp);
	} else if (DBX(db205, idx205 + 0U)) {
		target->set_status(GantryStatus::WindingOut);
	} else if (DBX(db205, idx205 + 1U)) {
		target->set_status(GantryStatus::WindingUp);
	} else {
		target->set_status(GantryStatus::Default);
	}
}

/*************************************************************************************************/
void WarGrey::SCADA::DI_winch(Winchlet* target, const uint8* db4, WinchLimits& limits, const uint8* db205, WinchDetails& details) {
	bool slack = (limits.slack > 0U) && DBX(db4, limits.slack - 1U);
	bool suction = (limits.suction > 0U) && DBX(db4, limits.suction - 1U);
	bool saddle = DBX(db4, limits.saddle - 1U);

	if (DBX(db4, limits.upper - 1U)) {
		target->set_status(WinchStatus::UpperLimited);
	} else if (slack || suction || saddle) {
		if (suction) {
			target->set_status(slack, WinchStatus::SuctionSlack, WinchStatus::SuctionLimited);
		} else if (saddle) {
			target->set_status(slack, WinchStatus::SaddleSlack, WinchStatus::SaddleLimited);
		} else {
			target->set_status(WinchStatus::Slack);
		}
	} else {
		unsigned int status = details.status - 1U;
		unsigned int sensor = details.sensor - 1U;
		bool can_windout = (DBX(db205, status + 4U));
		bool can_windup = (DBX(db205, status + 5U));
		bool fast = (details.draghead && DBX(db205, status + 7U));

		if (DBX(db205, status + 0U)) {
			target->set_status(fast, WinchStatus::FastWindingOut, WinchStatus::WindingOut);
		} else if (DBX(db205, status + 1U)) {
			target->set_status(fast, WinchStatus::FastWindingUp, WinchStatus::WindingUp);
		} else if (DBX(db205, sensor + 0U)) {
			target->set_status(WinchStatus::SensorUpperLimited);
		} else if (DBX(db205, sensor + 1U)) {
			target->set_status(WinchStatus::SensorLowerLimited);
		} else if (can_windout && can_windup) {
			target->set_status(fast, WinchStatus::FastWindReady, WinchStatus::WindReady);
		} else if (can_windout) {
			target->set_status(fast, WinchStatus::FastWindOutReady, WinchStatus::WindOutReady);
		} else if (can_windup) {
			target->set_status(fast, WinchStatus::FastWindUpReady, WinchStatus::WindUpReady);
		}

		// the rest are unused;
		//  target->set_status(DBX(db205, status + 2U), WinchStatus::Unlettable);
		//  target->set_status(DBX(db205, status + 3U), WinchStatus::Unpullable);
	}
}

void WarGrey::SCADA::DI_winch_override(Buttonlet* target, const uint8* db205, WinchDetails& details) {
	target->set_status(DBX(db205, details.override - 1U), ButtonStatus::Executing, ButtonStatus::Default);
}

void WarGrey::SCADA::DI_gantry(Gantrylet* target, const uint8* db4, unsigned int idx4_p1, const uint8* db205, unsigned int idx205_p1) {
	_DI_gantry(target, db4, idx4_p1 - 1U, db205, idx205_p1 - 1U);
}

void WarGrey::SCADA::DI_gantry(GantrySymbollet* target, const uint8* db4, unsigned int idx4_p1, const uint8* db205, unsigned int idx205_p1) {
	_DI_gantry(target, db4, idx4_p1 - 1U, db205, idx205_p1 - 1U);
}

void WarGrey::SCADA::DI_suction_buttons(Buttonlet* intarget, Buttonlet* detarget, const uint8* db205, unsigned int idx_p1) {
	if (DBX(db205, idx_p1 - 1U)) {
		intarget->set_status(ButtonStatus::Executing);
	} else if (DBX(db205, idx_p1 + 1U)) {
		intarget->set_status(ButtonStatus::Failed);
	} else if (DBX(db205, idx_p1 + 3U)) {
		intarget->set_status(ButtonStatus::Ready);
	} else {
		intarget->set_status(ButtonStatus::Default);
	}

	if (DBX(db205, idx_p1 + 0U)) {
		detarget->set_status(ButtonStatus::Executing);
	} else if (DBX(db205, idx_p1 + 2U)) {
		detarget->set_status(ButtonStatus::Failed);
	} else if (DBX(db205, idx_p1 + 4U)) {
		detarget->set_status(ButtonStatus::Ready);
	} else {
		detarget->set_status(ButtonStatus::Default);
	}
}

void WarGrey::SCADA::DI_ctension_button(Buttonlet* target, const uint8* db205, unsigned int idx_p1) {
	target->set_status(DBX(db205, idx_p1 - 1U), ButtonStatus::Executing, ButtonStatus::Default);
}

bool WarGrey::SCADA::DI_long_sb_drag(const uint8* db205) {
	return DBX(db205, 1417U - 1U);
}
