#include "schema/datalet/alarm_tbl.hpp"
#include "stone/tongue/alarm.hpp"
#include "dbmisc.hpp"

using namespace WarGrey::SCADA;

using namespace Concurrency;

using namespace Windows::Foundation;
using namespace Windows::System;
using namespace Windows::Storage;

using namespace Microsoft::Graphics::Canvas::Brushes;

static int alarm_busy_handler(void* args, int count) {
	// keep trying until it works
	return 1;
}

/*************************************************************************************************/
float WarGrey::SCADA::alarm_column_width_configure(unsigned int idx, unsigned int total) {
	float percentage = 1.0F / float(total);
	
	switch (_E(AMS, idx)) {
	case AMS::Event: percentage = 0.4F; break;
	case AMS::AlarmTime: case AMS::FixedTime: percentage = 0.2F; break;
	case AMS::Code: case AMS::Type: percentage = 0.1F; break;
	}

	return percentage;
}

long long WarGrey::SCADA::alarm_salt(Alarm& alarm) {
	long long salt = alarm.uuid; // see pk64_timestamp();

	return salt;
}

void WarGrey::SCADA::alarm_cell_style_configure(unsigned int idx, long long salt, TableCellStyle* style) {
	switch (_E(AMS, idx)) {
	case AMS::Event: style->align_fx = 0.0F; break;
	default: style->align_fx = 0.5F; break;
	}

	style->align_fy = 0.5F;
	style->background_color = Colours::Background;
	style->background_color = Colours::GhostWhite;
}

void WarGrey::SCADA::alarm_extract(Alarm& alarm, Platform::String^ fields[]) {
	unsigned int index = (unsigned int)alarm.index;

	fields[_I(AMS::Code)] = alarm_index_to_code(index).ToString();
	fields[_I(AMS::Event)] = Alarms::fromIndex(index)->ToLocalString();
	fields[_I(AMS::Type)] = alarm.type.value_or(1000LL).ToString();
	fields[_I(AMS::AlarmTime)] = make_timestamp_utc(alarm.alarmtime / 1000LL, true);
	fields[_I(AMS::FixedTime)] = (alarm.fixedtime.has_value() ? make_timestamp_utc(alarm.fixedtime.value() / 1000LL, true) : "");
}

void WarGrey::SCADA::alarm_index_translate(unsigned int index, unsigned int* db, unsigned int* dbx) {
	SET_BOX(db, index >> 16);
	SET_BOX(dbx, index & 0xFF);
}

unsigned int WarGrey::SCADA::alarm_index_to_code(unsigned int index) {
	unsigned int db, dbx;

	alarm_index_translate(index, &db, &dbx);

	if (db == 4) {
		dbx += 4U * 8U;
	} else {
		dbx += 204U * 8U;
	}

	return dbx;
}

/*************************************************************************************************/
private class AlarmCursor : public IAlarmCursor {
public:
	AlarmCursor(ITableDataReceiver* receiver, long long request_count, long long loaded_count)
		: receiver(receiver), request_count(request_count), loaded_count(loaded_count) {}

public:
	bool step(Alarm& alarm, bool asc, int code) override {
		bool go_on = (this->loaded_count < this->request_count);
		
		if (go_on) {
			alarm_extract(alarm, this->tempdata);
			this->receiver->on_row_datum(this->request_count, ++this->loaded_count, alarm_salt(alarm), this->tempdata, _N(AMS));
		}

		return go_on;
	}

public:
	long long loaded_count;

private:
	Platform::String^ tempdata[_N(AMS)];
	ITableDataReceiver* receiver;
	long long request_count;
};

/*************************************************************************************************/
AlarmDataSource::AlarmDataSource(Syslog* logger, RotationPeriod period, unsigned int period_count, unsigned int search_max)
	: RotativeSQLite3("alarm", logger, period, period_count), search_file_count_max(search_max), request_count(0LL) {}

AlarmDataSource::~AlarmDataSource() {
	this->cancel();

	if (this->dbc != nullptr) {
		delete this->dbc;
	}
}

bool AlarmDataSource::ready() {
	return IRotativeDirectory::root_ready() && RotativeSQLite3::ready();
}

bool AlarmDataSource::loading() {
	return (this->request_count > 0LL);
}

void AlarmDataSource::cancel() {
	if (this->loading()) {
		this->watcher.cancel();
	}
}

void AlarmDataSource::on_database_rotated(WarGrey::SCADA::SQLite3* prev_dbc, WarGrey::SCADA::SQLite3* dbc, long long timepoint) {
	// TODO: move the temporary data from in-memory SQLite3 into the current SQLite3

	create_alarm(dbc, true);
	this->get_logger()->log_message(Log::Debug, L"current file: %S", dbc->filename().c_str());
}

void AlarmDataSource::save(long long timepoint_ms, unsigned int index, Alarm& alarm) {
	default_alarm(alarm);

	alarm.index = index;
	alarm.alarmtime = timepoint_ms;
	alarm.fixedtime = timepoint_ms;

	insert_alarm(this, alarm);
}

void AlarmDataSource::load(ITableDataReceiver* receiver, long long request_count) {
	if (!this->loading()) {
		long long start = this->resolve_timepoint(current_seconds());
		long long interval = -this->span_seconds();
		
		this->get_logger()->log_message(Log::Debug, "start loading from now");

		this->request_count = request_count;
		this->time0 = current_inexact_milliseconds();
		this->do_loading_async(receiver, start, interval, 0LL, 0LL, 0LL, 0.0);
	}
}

void AlarmDataSource::do_loading_async(ITableDataReceiver* receiver, long long start, long long interval
	, unsigned int actual_file_count, unsigned int search_file_count, long long total, double span_ms) {
	bool asc = (interval > 0);

	if ((total >= this->request_count) || (search_file_count > this->search_file_count_max)) {
		double span_total = current_inexact_milliseconds() - this->time0;

		this->get_logger()->log_message(Log::Debug, L"loaded %d records from %d file(s) within %lfms(wasted: %lfms)",
			total, actual_file_count, span_total, span_total - span_ms);

		receiver->on_maniplation_complete(this->request_count);
		this->request_count = 0LL;
	} else {
		Platform::String^ dbsource = this->resolve_filename(start);
		cancellation_token token = this->watcher.get_token();

		create_task(this->rootdir()->TryGetItemAsync(dbsource), token).then([=](task<IStorageItem^> getting) {
			IStorageItem^ db = getting.get();
			long long next_timepoint = start + interval;

			if ((db != nullptr) && (db->IsOfType(StorageItemTypes::File))) {
				AlarmCursor acursor(receiver, this->request_count, total);
				double ms = current_inexact_milliseconds();
				
				this->dbc = new SQLite3(db->Path->Data(), this->get_logger());
				this->dbc->set_busy_handler(alarm_busy_handler);

				receiver->begin_maniplation_sequence();
				foreach_alarm(this->dbc, &acursor, this->request_count - total, 0, alarm::alarmtime, asc);
				receiver->end_maniplation_sequence();

				delete this->dbc;
				this->dbc = nullptr;

				ms = current_inexact_milliseconds() - ms;
				this->get_logger()->log_message(Log::Debug, L"loaded %d record(s) from[%s] within %lfms",
					acursor.loaded_count - total, dbsource->Data(), ms);

				this->do_loading_async(receiver, next_timepoint, interval,
					actual_file_count + 1U, search_file_count + 1U, acursor.loaded_count, span_ms + ms);
			} else {
				this->get_logger()->log_message(Log::Debug, L"skip non-existent source[%s]", dbsource->Data());
				this->do_loading_async(receiver, next_timepoint, interval,
					actual_file_count, search_file_count + 1U, total, span_ms);
			}
		}).then([=](task<void> check_exn) {
			try {
				check_exn.get();
			} catch (Platform::Exception^ e) {
				this->on_exception(e);
			} catch (task_canceled&) {}
		});
	}
}
