#include "schema/dbtest.hpp"
#include "schema/event.hpp"

using namespace WarGrey::SCADA;

void WarGrey::SCADA::dbtest(SQLite3* target) {
	auto sqlite3 = ((target == nullptr) ? new SQLite3() : target);
	AlarmEvent fevent = make_event("Info", "Yacht");
	AlarmEvent events[2];
	AlarmEvent_pk id = event_identity(fevent);

	default_event(events[0], "Error", "Fire");
	default_event(events[1], "Fatal", "Propeller");

	create_event(sqlite3, true);
	sqlite3->table_info("sqlite_master");
	sqlite3->table_info("event");

	insert_event(sqlite3, fevent);
	insert_event(sqlite3, events);

	auto aes = list_event(sqlite3);
	for (auto lt = aes.begin(); lt != aes.end(); lt++) {
		AlarmEvent_pk pk = (*lt);
		std::optional<AlarmEvent> maybe_e = seek_event(sqlite3, pk);

		if (maybe_e.has_value()) {
			AlarmEvent e = maybe_e.value();

			sqlite3->get_logger()->log_message(Log::Info,
				L"%lld, %S, %S, %lld, %lld",
				e.uuid, e.name.c_str(), e.type.c_str(),
				e.ctime.value_or(false), e.mtime.value_or(false));
		}
	}

	events[0].type = "Fatal";
	events[0].uuid = 42;
	update_event(sqlite3, events[0]);

	sqlite3->get_logger()->log_message(Log::Warning,
		L"alarm count: %lld, average: %lf, earliest: %lld, lastest: %lld",
		event_count(sqlite3), event_average(sqlite3),
		(int64)(event_min(sqlite3, event::mtime).value_or(0.0)),
		(int64)(event_max(sqlite3, event::mtime).value_or(0.0)));

	delete_event(sqlite3, id);
	if (!seek_event(sqlite3, id).has_value()) {
		sqlite3->get_logger()->log_message(Log::Info, "`seek_table` works for absent record");
	}

	sqlite3->list_tables();
	drop_event(sqlite3);
	sqlite3->table_info("event");
}
