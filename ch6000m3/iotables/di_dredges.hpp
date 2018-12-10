#pragma once

#include "graphlet/device/gantrylet.hpp"
#include "graphlet/buttonlet.hpp"

namespace WarGrey::SCADA {
	// DB4, starts from 1
	static unsigned int gantry_ps_trunnion_limited = 325U;
	static unsigned int gantry_ps_intermediate_limited = 357U;
	static unsigned int gantry_ps_draghead_limited = 365U;

	static unsigned int gantry_sb_trunnion_limited = 341U;
	static unsigned int gantry_sb_intermediate_limited = 389U;
	static unsigned int gantry_sb_long_draghead_limited = 409U;
	static unsigned int gantry_sb_short_draghead_limited = 397U;

	// DB205, starts from 1
	static unsigned int gantry_ps_trunnion_details = 1305U;
	static unsigned int gantry_ps_intermediate_details = 1313U;
	static unsigned int gantry_ps_draghead_details = 1321U;

	static unsigned int gantry_ps_trunnion_virtual_up_limited = 2041U;
	static unsigned int gantry_ps_trunnion_virtual_out_limited = 2042U;
	static unsigned int gantry_ps_intermediate_virtual_up_limited = 2043U;
	static unsigned int gantry_ps_intermediate_virtual_out_limited = 2044U;
	static unsigned int gantry_ps_draghead_virtual_up_limited = 2045U;
	static unsigned int gantry_ps_draghead_virtual_out_limited = 2046U;

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

	static unsigned int ps_almo_auto = 1872U;
	static unsigned int sb_almo_auto = 1880U;

	/************************************************************************************************/
	void DI_gantry(WarGrey::SCADA::Gantrylet* target, const uint8* db4, unsigned int idx4_p1, const uint8* db205, unsigned int idx205_p1);
	void DI_gantry(WarGrey::SCADA::GantrySymbollet* target, const uint8* db4, unsigned int idx4_p1, const uint8* db205, unsigned int idx205_p1);

	void DI_suction_buttons(WarGrey::SCADA::Buttonlet* intarget, WarGrey::SCADA::Buttonlet* detarget, const uint8* db205, unsigned int idx_p1);
	void DI_boolean_button(WarGrey::SCADA::Buttonlet* target, const uint8* db205, unsigned idx_p1);

	bool DI_long_sb_drag(const uint8* db205);
}
