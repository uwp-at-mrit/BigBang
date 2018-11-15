#pragma once

#include "graphlet/device/winchlet.hpp"
#include "graphlet/device/gantrylet.hpp"

namespace WarGrey::SCADA {
	private struct WinchLimits {
	public:
		WinchLimits(unsigned int upper, unsigned int support, unsigned int slack = 0U, unsigned int suction = 0U)
			: upper(upper), support(support), suction(suction), slack(slack) {}

	public:
		unsigned int upper;
		unsigned int support;
		unsigned int suction;
		unsigned int slack;
	};

	private struct WinchDetails {
	public:
		WinchDetails(unsigned int status, unsigned int sensor, unsigned int control, bool draghead)
			: status(status), sensor(sensor), control(control), draghead(draghead) {}

	public:
		unsigned int status;
		unsigned int sensor;
		unsigned int control;
		bool draghead;
	};

	// DB4, starts from 1
	static WarGrey::SCADA::WinchLimits winch_ps_trunnion_limits = WarGrey::SCADA::WinchLimits(321U, 322U, 323U, 324U);
	static WarGrey::SCADA::WinchLimits winch_ps_intermediate_limits = WarGrey::SCADA::WinchLimits(332U, 354U);
	static WarGrey::SCADA::WinchLimits winch_ps_draghead_limits = WarGrey::SCADA::WinchLimits(361U, 362U);
	static unsigned int gantry_ps_trunnion_limited = 325U;
	static unsigned int gantry_ps_intermediate_limited = 357U;
	static unsigned int gantry_ps_draghead_limited = 365U;

	static WarGrey::SCADA::WinchLimits winch_sb_trunnion_limits = WarGrey::SCADA::WinchLimits(337U, 338U, 339U, 340U);
	static WarGrey::SCADA::WinchLimits winch_sb_intermediate_limits = WarGrey::SCADA::WinchLimits(348U, 386U);
	static WarGrey::SCADA::WinchLimits winch_sb_draghead_limits = WarGrey::SCADA::WinchLimits(393U, 394U);
	static unsigned int gantry_sb_trunnion_limited = 341U;
	static unsigned int gantry_sb_intermediate_limited = 389U;
	static unsigned int gantry_sb_long_draghead_limited = 409U;
	static unsigned int gantry_sb_short_draghead_limited = 397U;

	// DB205, starts from 1
	static WarGrey::SCADA::WinchDetails winch_ps_trunnion_details = WarGrey::SCADA::WinchDetails(1353U, 1793U, 2033U, false);
	static WarGrey::SCADA::WinchDetails winch_ps_intermediate_details = WarGrey::SCADA::WinchDetails(1361U, 1795U, 2034U, false);
	static WarGrey::SCADA::WinchDetails winch_ps_draghead_details = WarGrey::SCADA::WinchDetails(1369U, 1797U, 2035U, true);
	static unsigned int gantry_ps_trunnion_details = 1305U;
	static unsigned int gantry_ps_intermediate_details = 1313U;
	static unsigned int gantry_ps_draghead_details = 1321U;

	static unsigned int gantry_ps_trunnion_virtual_up_limited = 2041U;
	static unsigned int gantry_ps_trunnion_virtual_out_limited = 2042U;
	static unsigned int gantry_ps_intermediate_virtual_up_limited = 2043U;
	static unsigned int gantry_ps_intermediate_virtual_out_limited = 2044U;
	static unsigned int gantry_ps_draghead_virtual_up_limited = 2045U;
	static unsigned int gantry_ps_draghead_virtual_out_limited = 2046U;

	static WarGrey::SCADA::WinchDetails winch_sb_trunnion_details = WarGrey::SCADA::WinchDetails(1377U, 1799U, 2036U, false);
	static WarGrey::SCADA::WinchDetails winch_sb_intermediate_details = WarGrey::SCADA::WinchDetails(1385U, 1801U, 2037U, false);
	static WarGrey::SCADA::WinchDetails winch_sb_draghead_details = WarGrey::SCADA::WinchDetails(1393U, 1803U, 2038U, true);
	static unsigned int gantry_sb_trunnion_details = 1329U;
	static unsigned int gantry_sb_intermediate_details = 1337U;
	static unsigned int gantry_sb_draghead_details = 1345U;

	static unsigned int gantry_sb_trunnion_virtual_up_limited = 2049U;
	static unsigned int gantry_sb_trunnion_virtual_out_limited = 2050U;
	static unsigned int gantry_sb_intermediate_virtual_up_limited = 2051U;
	static unsigned int gantry_sb_intermediate_virtual_out_limited = 2052U;
	static unsigned int gantry_sb_draghead_virtual_up_limited = 2053U;
	static unsigned int gantry_sb_draghead_virtual_out_limited = 2054U;

	/************************************************************************************************/
	template<class W>
	void DI_winch(W* target
		, const uint8* db4, WarGrey::SCADA::WinchLimits& limits
		, const uint8* db205, WarGrey::SCADA::WinchDetails& details) {
		bool slack = (limits.slack > 0U) && DBX(db4, limits.slack - 1U);
		
		target->set_remote_control(DBX(db205, details.control - 1U));

		if (DBX(db4, limits.upper - 1U)) {
			target->set_status(WinchStatus::UpperLimited);
		} else if (DBX(db4, limits.support - 1U)) {
			target->set_status(slack, WinchStatus::SaddleSlack, WinchStatus::SaddleLimited);
		} else if ((limits.suction > 0U) && DBX(db4, limits.suction - 1U)) {
			target->set_status(slack, WinchStatus::SuctionSlack, WinchStatus::SuctionLimited);
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

	template<class G>
	void DI_gantry(G* target, const uint8* db4, unsigned int idx4_p1, const uint8* db205, unsigned int idx205_p1) {
		if (DBX(db4, idx4_p1 - 1U)) {
			target->set_status(GantryStatus::WindedOut);
		} else if (DBX(db4, idx4_p1 + 0U)) {
			target->set_status(GantryStatus::WindedUp);
		} else if (DBX(db205, idx205_p1 - 1U)) {
			target->set_status(GantryStatus::WindingOut);
		} else if (DBX(db205, idx205_p1 + 0U)) {
			target->set_status(GantryStatus::WindingUp);
		} else {
			target->set_status(GantryStatus::Default);
		}
	}

	bool DI_long_sb_drag(const uint8* db205) {
		return DBX(db205, 1417U - 1U);
	}

	template<class G, typename Menu>
	bool winch_command_executable(G* target, Menu cmd, bool draghead, bool otherwise) {
		WinchStatus status = target->get_status();
		bool executable = otherwise;

		if (cmd == Menu::HighSpeed) {
			if (!draghead) {
				executable = false;
			}
		}

		return executable;
	}

	template<class G, typename Menu>
	bool gantry_command_executable(G* target, Menu cmd, bool otherwise) {
		GantryStatus status = target->get_status();
		bool executable = otherwise;

		return executable;
	}
}
