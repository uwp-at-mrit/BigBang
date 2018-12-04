#include <ppltasks.h>

#include "schema/timeseries/earthwork_ds.hpp"
#include "schema/earthwork.hpp"
#include "dbmisc.hpp"

using namespace WarGrey::SCADA;

using namespace Concurrency;

using namespace Windows::Foundation;
using namespace Windows::System;
using namespace Windows::Storage;

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

private class EarthWorkCursor : public IEarthWorkCursor {
public:
	EarthWorkCursor(ITimeSeriesDataReceiver* receiver, long long open_s, long long close_s) : receiver(receiver) {
		if (open_s < close_s) {
			this->open_timepoint = open_s * 1000LL;
			this->close_timepoint = close_s * 1000LL;
		} else {
			this->open_timepoint = close_s * 1000LL;
			this->close_timepoint = open_s * 1000LL;
		}
	}

public:
	bool step(EarthWork& ework) {
		bool okay = ((ework.timestamp >= this->open_timepoint) && (ework.timestamp <= this->close_timepoint));

		if (okay) {
			this->tempdata[_I(EWTS::EarthWork)] = ework.product;
			this->tempdata[_I(EWTS::Vessel)] = ework.vessel;
			this->tempdata[_I(EWTS::HopperHeight)] = ework.hopper_height;
			this->tempdata[_I(EWTS::Loading)] = ework.loading;
			this->tempdata[_I(EWTS::Displacement)] = ework.displacement;

			this->receiver->on_datum_values(ework.timestamp, tempdata, _N(EWTS));

			this->count++;
		}

		this->total++;

		return true;
	}

public:
	unsigned int count;
	unsigned int total;

private:
	double tempdata[_N(EWTS)];
	ITimeSeriesDataReceiver* receiver;
	long long open_timepoint;
	long long close_timepoint;
};

/*************************************************************************************************/
EarthWorkDataSource::EarthWorkDataSource(Syslog* logger, RotationPeriod period, unsigned int period_count)
	: RotativeSQLite3("earthwork", logger, period, period_count), open_timepoint(0LL) {}

bool EarthWorkDataSource::ready() {
	return IRotativeDirectory::root_ready() && RotativeSQLite3::ready();
}

bool EarthWorkDataSource::loading() {
	return (this->open_timepoint > 0LL);
}

void EarthWorkDataSource::on_database_rotated(WarGrey::SCADA::SQLite3* prev_dbc, WarGrey::SCADA::SQLite3* dbc) {
	// TODO: move the temporary data from in-memory SQLite3 into the current SQLite3

	create_earthwork(dbc, true);
	this->get_logger()->log_message(Log::Notice, L"current file: %S", dbc->filename().c_str());
}

void EarthWorkDataSource::load(ITimeSeriesDataReceiver* receiver, long long open_s, long long close_s) {
	if (!this->loading()) {
		long long interval = this->span_seconds() * ((open_s < close_s) ? 1LL : -1LL);
		
		this->open_timepoint = open_s;
		this->time0 = current_inexact_milliseconds();
		this->do_loading_async(receiver, open_s, close_s, interval, 0LL, 0LL, 0.0);
	}
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

void EarthWorkDataSource::do_loading_async(ITimeSeriesDataReceiver* receiver
	, long long open_s, long long close_s, long long interval
	, unsigned int file_count, unsigned int total, double span_ms) {
	bool asc = (interval > 0);
	
	if ((asc ? (open_s > close_s) : (open_s < close_s))) {
		double span_total = current_inexact_milliseconds() - this->time0;

		this->get_logger()->log_message(Log::Info, L"loaded %d records from %d file(s) within %lfms(delta: %lfms)",
			total, file_count, span_total, span_total - span_ms);

		receiver->on_maniplation_complete(this->open_timepoint, close_s);
		this->open_timepoint = 0LL;
	} else {
		Platform::String^ dbsource = this->resolve_filename(open_s);

		create_task(this->rootdir()->TryGetItemAsync(dbsource)).then([=](task<IStorageItem^> getting) {
			IStorageItem^ db = getting.get();

			if ((db != nullptr) && (db->IsOfType(StorageItemTypes::File))) {
				EarthWorkCursor ecursor(receiver, open_s, close_s);
				double ms = current_inexact_milliseconds();
				SQLite3* dbc = new SQLite3(db->Path->Data(), this->get_logger());

				receiver->begin_maniplation_sequence();
				foreach_earthwork(dbc, &ecursor, 0, 0, earthwork::timestamp, asc);
				receiver->end_maniplation_sequence();

				delete dbc;

				ms = current_inexact_milliseconds() - ms;
				this->get_logger()->log_message(Log::Info, L"loaded %d[%d] record(s) from[%s] within %lfms",
					ecursor.count, ecursor.total, dbsource->Data(), ms);

				this->do_loading_async(receiver, open_s + interval, close_s, interval,
					file_count + 1LL, total + ecursor.count, span_ms + ms);
			} else {
				this->get_logger()->log_message(Log::Info, L"skip non-existent source[%s]", dbsource->Data());
				this->do_loading_async(receiver, open_s + interval, close_s, interval, file_count, total, span_ms);
			}
		}).then([=](task<void> check_exn) {
			try {
				check_exn.get();
			} catch (Platform::Exception^ e) {
				this->on_exception(e);
			}
		});
	}
}
