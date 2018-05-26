#include "event.hpp"

#include "dbsystem.hpp"
#include "dbtypes.hpp"

using namespace WarGrey::SCADA;

static Platform::String^ event_rowids[] = { "uuid" };

static TableColumnInfo event_columns[] = {
    { "uuid", SDT::Integer, nullptr, DB_PRIMARY_KEY | 0 | 0 },
    { "type", SDT::Text, nullptr, 0 | DB_NOT_NULL | 0 },
    { "name", SDT::Text, nullptr, 0 | DB_NOT_NULL | DB_UNIQUE },
    { "ctime", SDT::Integer, nullptr, 0 | 0 | 0 },
    { "mtime", SDT::Integer, nullptr, 0 | 0 | 0 },
};

void WarGrey::SCADA::restore_event(IPreparedStatement* stmt, AlarmEvent& self) {
    self.uuid = stmt->column_int64(0U);
    self.type = stmt->column_text(1U);
    self.name = stmt->column_text(2U);
    self.ctime = stmt->column_int64(3U);
    self.mtime = stmt->column_int64(4U);
}

void WarGrey::SCADA::create_event(IDBSystem* dbc, bool if_not_exists) {
    IVirtualSQL* vsql = dbc->make_sql_factory(event_columns, sizeof(event_columns)/sizeof(TableColumnInfo));
    Platform::String^ sql = vsql->create_table("event", event_rowids, sizeof(event_rowids)/sizeof(Platform::String^), if_not_exists);

    dbc->exec(sql);
}

void WarGrey::SCADA::insert_event(IDBSystem* dbc, AlarmEvent* self, bool replace) {
    insert_event(dbc, self, 1, replace);
}

void WarGrey::SCADA::insert_event(IDBSystem* dbc, AlarmEvent* selves, int count, bool replace) {
    IVirtualSQL* vsql = dbc->make_sql_factory(event_columns, sizeof(event_columns)/sizeof(TableColumnInfo));
    Platform::String^ sql = vsql->insert_into("event", replace);
    IPreparedStatement* stmt = dbc->prepare(sql);

    if (stmt != nullptr) {
        for (int i = 0; i < count; i ++) {
            stmt->bind_parameter(0, selves[i].uuid);
            stmt->bind_parameter(1, selves[i].type);
            stmt->bind_parameter(2, selves[i].name);
            stmt->bind_parameter(3, selves[i].ctime);
            stmt->bind_parameter(4, selves[i].mtime);

            dbc->exec(stmt);
            stmt->clear_bindings();
        }

        delete stmt;
    }
}

std::list<AlarmEvent> WarGrey::SCADA::select_event(IDBSystem* dbc, unsigned int limit, unsigned int offset) {
    IVirtualSQL* vsql = dbc->make_sql_factory(event_columns, sizeof(event_columns)/sizeof(TableColumnInfo));
    Platform::String^ sql = vsql->select_from("event", limit, offset);
    IPreparedStatement* stmt = dbc->prepare(sql);
    std::list<AlarmEvent> queries;

    if (stmt != nullptr) {
        AlarmEvent self;

        while(stmt->step()) {
            restore_event(stmt, self);
            queries.push_back(self);
        }

        delete stmt;
    }

    return queries;
}

void WarGrey::SCADA::drop_event(IDBSystem* dbc) {
    IVirtualSQL* vsql = dbc->make_sql_factory(event_columns, sizeof(event_columns)/sizeof(TableColumnInfo));
    Platform::String^ sql = vsql->drop_table("event");

    dbc->exec(sql);
}

