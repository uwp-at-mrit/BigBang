#include "schema/timeseries/earthwork_ds.hpp"
#include "schema/earthwork.hpp"
#include "dbmisc.hpp"

using namespace WarGrey::SCADA;

using namespace Windows::Foundation;
using namespace Windows::System;

using namespace Microsoft::Graphics::Canvas::Brushes;

/*************************************************************************************************/
ICanvasBrush^ WarGrey::SCADA::earthwork_line_color_dictionary(unsigned int index) {
	ICanvasBrush^ color = nullptr;

	switch (_E(EWTS, index % _N(EWTS))) {
	case EWTS::EarthWork: color = Colours::Khaki; break;
	case EWTS::Vessel: color = Colours::Cyan; break;
	case EWTS::HopperHeight: color = Colours::Crimson; break;
	case EWTS::Loading: color = Colours::Orange; break;
	case EWTS::Displacement: color = Colours::MediumSeaGreen; break;
	}

	return color;
}

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

void EarthWorkDataSource::load(WarGrey::SCADA::ITimeSeriesDataReceiver* receiver, long long open, long long closed) {
	//this->get_logger()->log_message(Log::Info, L"loading from %s to %s",
		//this->resolve_pathname(open)->Data(),
		//this->resolve_pathname(closed)->Data());
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
