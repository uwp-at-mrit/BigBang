#include "schema/dbtest.hpp"
#include "schema/event.hpp"

#include "cast.hpp"

using namespace WarGrey::SCADA;

private enum class dbevent { Yacht, Propulsion, Propeller };

void WarGrey::SCADA::dbtest(SQLite3* target) {
	auto sqlite3 = ((target == nullptr) ? new SQLite3() : target);
	AlarmEvent fevent = make_event(_I(dbevent::Yacht), 1);
	AlarmEvent events[2];
	AlarmEvent_pk id = event_identity(fevent);

	default_event(events[0], _I(dbevent::Propulsion), 0);
	default_event(events[1], _I(dbevent::Propeller), 2);

	create_event(sqlite3, false);
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
				L"%X, %S, %s, %lld, %d, %S",
				e.uuid, _E(dbevent, e.name).ToString()->Data(),
				_E(Log, e.status).ToString()->Data(),
				e.timestamp, e.code.value_or(0),
				e.note.value_or("Undefined").c_str());
		}
	}

	events[0].status = 5;
	events[0].uuid = 42;
	update_event(sqlite3, events[0]);

	sqlite3->get_logger()->log_message(Log::Warning,
		L"alarm count: %lld, average: %lf, earliest: %lld, lastest: %lld",
		event_count(sqlite3), event_average(sqlite3),
		(int64)(event_min(sqlite3, event::uuid).value_or(0.0)),
		(int64)(event_max(sqlite3, event::uuid).value_or(0.0)));

	delete_event(sqlite3, id);
	if (!seek_event(sqlite3, id).has_value()) {
		sqlite3->get_logger()->log_message(Log::Info, "`seek_table` works for absent record");
	}

	sqlite3->list_tables();
	drop_event(sqlite3);
	sqlite3->table_info("event");
	create_event(sqlite3, true);
}
