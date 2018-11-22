#pragma once

#include "graphlet/device/winchlet.hpp"
#include "graphlet/device/gantrylet.hpp"
#include "graphlet/buttonlet.hpp"

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
		WinchDetails(unsigned int status, unsigned int sensor, unsigned int override, bool draghead)
			: status(status), sensor(sensor), override(override), draghead(draghead) {}

	public:
		unsigned int status;
		unsigned int sensor;
		unsigned int override;
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

	static unsigned int suction_ps_buttons = 1913U;
	static unsigned int suction_sb_buttons = 1921U;

	static unsigned int ctension_ps_button = 1569U;
	static unsigned int ctension_sb_button = 1570U;

	/************************************************************************************************/
	void DI_winch(WarGrey::SCADA::Winchlet* target
		, const uint8* db4, WarGrey::SCADA::WinchLimits& limits
		, const uint8* db205, WarGrey::SCADA::WinchDetails& details);

	void DI_winch_override(WarGrey::SCADA::Buttonlet* target, const uint8* db205, WarGrey::SCADA::WinchDetails& details);

	void DI_gantry(WarGrey::SCADA::Gantrylet* target, const uint8* db4, unsigned int idx4_p1, const uint8* db205, unsigned int idx205_p1);
	void DI_gantry(WarGrey::SCADA::GantrySymbollet* target, const uint8* db4, unsigned int idx4_p1, const uint8* db205, unsigned int idx205_p1);

	void DI_suction_buttons(WarGrey::SCADA::Buttonlet* intarget, WarGrey::SCADA::Buttonlet* detarget, const uint8* db205, unsigned int idx_p1);
	void DI_ctension_button(WarGrey::SCADA::Buttonlet* target, const uint8* db205, unsigned int idx_p1);

	bool DI_long_sb_drag(const uint8* db205);

	template<typename Menu>
	bool winch_command_executable(WarGrey::SCADA::Winchlet* target, Menu cmd, bool draghead, bool otherwise) {
		WinchStatus status = target->get_status();
		bool executable = otherwise;

		if (cmd == Menu::HighSpeed) {
			if (!draghead) {
				executable = false;
			}
		}

		return executable;
	}

	template<typename Menu>
	bool gantry_command_executable(WarGrey::SCADA::Gantrylet* target, Menu cmd, bool otherwise) {
		GantryStatus status = target->get_status();
		bool executable = otherwise;

		return executable;
	}
}
