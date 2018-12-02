#include "schema/timeseries/earthwork_ds.hpp"

#include "graphlet/dashboard/timeserieslet.hpp"

using namespace WarGrey::SCADA;

using namespace Windows::Foundation;
using namespace Windows::System;

EarthWorkDataSource::EarthWorkDataSource(Syslog* logger, RotationPeriod period, unsigned int period_count)
	: RotativeSQLite3("earthwork", logger, period, period_count) {}

bool EarthWorkDataSource::ready() {
	return RotativeSQLite3::ready();
}

void EarthWorkDataSource::on_database_rotated(WarGrey::SCADA::SQLite3* prev_dbc, WarGrey::SCADA::SQLite3* current_dbc) {

}

void EarthWorkDataSource::save(long long timepoint, double* values, unsigned int n) {
	this->get_logger()->log_message(Log::Info, L"timepoint: %lld", timepoint);
}


