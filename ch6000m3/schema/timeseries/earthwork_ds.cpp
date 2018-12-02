#include "schema/timeseries/earthwork_ds.hpp"
#include "schema/earthwork.hpp"
#include "dbmisc.hpp"

using namespace WarGrey::SCADA;

using namespace Windows::Foundation;
using namespace Windows::System;

/*************************************************************************************************/
EarthWorkDataSource::EarthWorkDataSource(Syslog* logger, RotationPeriod period, unsigned int period_count)
	: RotativeSQLite3("earthwork", logger, period, period_count) {}

bool EarthWorkDataSource::ready() {
	return RotativeSQLite3::ready();
}

void EarthWorkDataSource::on_database_rotated(WarGrey::SCADA::SQLite3* prev_dbc, WarGrey::SCADA::SQLite3* dbc) {
	// TODO: move the temporary data from in-memory SQLite3 into the current SQLite3

	create_earthwork(dbc, true);
}

void EarthWorkDataSource::save(long long timepoint, double* values, unsigned int n) {
	EarthWork ework;

	ework.uuid = pk64_timestamp();
	ework.timestamp = timepoint;

	for (unsigned int i = 0; i < n; i++) {
		switch (_E(EWTS, i)) {
		case EWTS::EarthWork: ework.product = values[i]; break;
		case EWTS::Vessel : ework.vessel = values[i]; break;
		case EWTS::HopperHeight: ework.hopper_height = values[i]; break;
		case EWTS::Loading: ework.loading = values[i]; break;
		case EWTS::Displacement: ework.displacement = values[i]; break;
		}
	}

	insert_earthwork(this, ework);
}
