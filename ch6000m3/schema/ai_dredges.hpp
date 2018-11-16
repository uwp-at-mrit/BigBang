#pragma once

namespace WarGrey::SCADA {
	private struct DredgeAddress {
		unsigned int drag_position;
		unsigned int intermediate_angle;
		unsigned int draghead_angle;
		unsigned int visor_angle;

		unsigned int compensator;
		unsigned int density_speed;
		unsigned int pulling_force;

		unsigned int vacuum_pressure;
		unsigned int discharge_pressure;
		unsigned int differential_pressure;
		unsigned int suction_inflator_pressure;
		
		unsigned int winch_length;
		unsigned int winch_speed;
	};

	WarGrey::SCADA::DredgeAddress* make_ps_dredging_system_schema();
	WarGrey::SCADA::DredgeAddress* make_sb_dredging_system_schema();

	// DB203, real data
	static unsigned int winch_ps_trunnion_A_pressure = 56U;
	static unsigned int winch_ps_trunnion_B_pressure = 57U;
	static unsigned int winch_ps_intermediate_A_pressure = 58U;
	static unsigned int winch_ps_intermediate_B_pressure = 59U;
	static unsigned int winch_ps_draghead_A_pressure = 80U;
	static unsigned int winch_ps_draghead_B_pressure = 81U;

	static unsigned int winch_sb_trunnion_A_pressure = 60U;
	static unsigned int winch_sb_trunnion_B_pressure = 61U;
	static unsigned int winch_sb_intermediate_A_pressure = 62U;
	static unsigned int winch_sb_intermediate_B_pressure = 63U;
	static unsigned int winch_sb_draghead_A_pressure = 96U;
	static unsigned int winch_sb_draghead_B_pressure = 97U;
	
	static unsigned int winch_ps_intermediate_remote_speed = 131U;
	static unsigned int winch_ps_draghead_remote_speed = 130U;

	static unsigned int winch_sb_intermediate_remote_speed = 146U;
	static unsigned int winch_sb_draghead_remote_speed = 147U;

	static unsigned int ps_draghead_differential_pressure = 109U;
	static unsigned int sb_draghead_differential_pressure = 125U;
}
