#pragma once

#include "graphlet/device/winchlet.hpp"

namespace WarGrey::SCADA {
	private struct WinchLimits {
	public:
		WinchLimits(unsigned int upper, unsigned int saddle, unsigned int slack = 0U, unsigned int suction = 0U)
			: upper(upper), saddle(saddle), suction(suction), slack(slack) {}

	public:
		unsigned int upper;
		unsigned int saddle;
		unsigned int suction;
		unsigned int slack;
	};

	private struct WinchDetails {
	public:
		WinchDetails(unsigned int status, unsigned int soft_upper, unsigned int soft_lower, unsigned int override, bool draghead)
			: status(status), soft_upper(soft_upper), soft_lower(soft_lower), override(override), draghead(draghead) {}

	public:
		unsigned int status;
		unsigned int soft_upper;
		unsigned int soft_lower;
		unsigned int override;
		bool draghead;
	};

	// DB4, starts from 1
	static WarGrey::SCADA::WinchLimits winch_ps_trunnion_limits = WarGrey::SCADA::WinchLimits(321U, 322U, 323U, 324U);
	static WarGrey::SCADA::WinchLimits winch_ps_intermediate_limits = WarGrey::SCADA::WinchLimits(332U, 354U);
	static WarGrey::SCADA::WinchLimits winch_ps_draghead_limits = WarGrey::SCADA::WinchLimits(361U, 362U);
	static unsigned int winch_ps_trunnion_feedback = 169U;
	static unsigned int winch_ps_intermediate_feedback = 177U;
	static unsigned int winch_ps_draghead_feedback = 185U;

	static WarGrey::SCADA::WinchLimits winch_sb_trunnion_limits = WarGrey::SCADA::WinchLimits(337U, 338U, 339U, 340U);
	static WarGrey::SCADA::WinchLimits winch_sb_intermediate_limits = WarGrey::SCADA::WinchLimits(348U, 386U);
	static WarGrey::SCADA::WinchLimits winch_sb_draghead_limits = WarGrey::SCADA::WinchLimits(393U, 394U);
	static unsigned int winch_sb_trunnion_feedback = 193U;
	static unsigned int winch_sb_intermediate_feedback = 201U;
	static unsigned int winch_sb_draghead_feedback = 209U;

	// DB205, starts from 1
	static WarGrey::SCADA::WinchDetails winch_ps_trunnion_details = WarGrey::SCADA::WinchDetails(1353U, 1793U, 1817U, 2033U, false);
	static WarGrey::SCADA::WinchDetails winch_ps_intermediate_details = WarGrey::SCADA::WinchDetails(1361U, 1795U, 1818U, 2034U, false);
	static WarGrey::SCADA::WinchDetails winch_ps_draghead_details = WarGrey::SCADA::WinchDetails(1369U, 1797U, 1819U, 2035U, true);
	
	static WarGrey::SCADA::WinchDetails winch_sb_trunnion_details = WarGrey::SCADA::WinchDetails(1377U, 1799U, 1820U, 2036U, false);
	static WarGrey::SCADA::WinchDetails winch_sb_intermediate_details = WarGrey::SCADA::WinchDetails(1385U, 1801U, 1821U, 2037U, false);
	static WarGrey::SCADA::WinchDetails winch_sb_draghead_details = WarGrey::SCADA::WinchDetails(1393U, 1803U, 1822U, 2038U, true);
	
	/************************************************************************************************/
	void DI_winch(WarGrey::SCADA::Winchlet* target
		, const uint8* db4, unsigned int feedback_p1, WarGrey::SCADA::WinchLimits& limits
		, const uint8* db205, WarGrey::SCADA::WinchDetails& details);

	bool winch_remote_control(const uint8* db4, unsigned int feedback_p1);
}
