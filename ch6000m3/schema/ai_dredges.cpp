#include "schema/ai_dredges.hpp"

using namespace WarGrey::SCADA;

static DredgeAddress* ps_schema;
static DredgeAddress* sb_schema;

/*************************************************************************************************/
DredgeAddress* WarGrey::SCADA::make_ps_dredging_system_schema() {
	if (ps_schema == nullptr) {
		DredgeAddress* schema = new DredgeAddress();

		schema->compensator = 82U;
		schema->density_speed = 172U;
		schema->pulling_force = 110U;

		schema->differential_pressure = 109U;
		schema->vacuum_pressure = 164U;
		schema->discharge_pressure = 165U;

		schema->winch_speed = 280U;
		schema->winch_length = 544U;

		schema->drag_position = 20U;
		schema->intermediate_angle = 408U;
		schema->draghead_angle = 412U;
		schema->visor_angle = 272U;

		ps_schema = schema;
	}

	return ps_schema;
}

DredgeAddress* WarGrey::SCADA::make_sb_dredging_system_schema() {
	if (sb_schema == nullptr) {
		DredgeAddress* schema = new DredgeAddress();

		schema->compensator = 98U;
		schema->density_speed = 174U;
		schema->pulling_force = 126U;

		schema->differential_pressure = 125U;
		schema->vacuum_pressure = 166U;
		schema->discharge_pressure = 167U;

		schema->winch_speed = 292U;
		schema->winch_length = 556U;

		schema->drag_position = 96U;
		schema->intermediate_angle = 416U;
		schema->draghead_angle = 420U;
		schema->visor_angle = 276U;

		sb_schema = schema;
	}

	return sb_schema;
}
