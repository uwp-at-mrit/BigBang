#pragma once

namespace WarGrey::SCADA {
	private struct DredgeAddress {
		unsigned int drag_position;
		unsigned int draghead_angle;
		unsigned int compensator_length;
		unsigned int density_speed;

		unsigned int vacuum_pressure;
		unsigned int discharge_pressure;
		unsigned int differential_pressure;
		
		unsigned int winch_length;
		unsigned int winch_speed;
	};

	WarGrey::SCADA::DredgeAddress* make_ps_dredging_system_schema();
	WarGrey::SCADA::DredgeAddress* make_sb_dredging_system_schema();
}
