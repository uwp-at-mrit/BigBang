#include "event.hpp"

#include "dbsystem.hpp"
#include "dbtypes.hpp"

using namespace WarGrey::SCADA;

static Platform::String^ event_rowids[] = { "uuid", "name" };

static TableColumnInfo event_columns[] = {
    { "uuid", SDT::Integer, nullptr, DB_PRIMARY_KEY | 0 | 0 },
    { "type", SDT::Text, nullptr, 0 | DB_NOT_NULL | 0 },
    { "name", SDT::Text, nullptr, DB_PRIMARY_KEY | DB_NOT_NULL | DB_UNIQUE },
    { "ctime", SDT::Integer, nullptr, 0 | 0 | 0 },
    { "mtime", SDT::Integer, nullptr, 0 | 0 | 0 },
};

void WarGrey::SCADA::create_event(IDBSystem* dbc, bool if_not_exists) {
    IVirtualSQL* vsql = dbc->make_sql_factory(event_columns, sizeof(event_columns)/sizeof(TableColumnInfo));
    Platform::String^ sql = vsql->create_table("event", event_rowids, sizeof(event_rowids)/sizeof(Platform::String^), if_not_exists);

    dbc->exec(sql);
}

void WarGrey::SCADA::drop_event(IDBSystem* dbc) {
    IVirtualSQL* vsql = dbc->make_sql_factory(event_columns, sizeof(event_columns)/sizeof(TableColumnInfo));
    Platform::String^ sql = vsql->drop_table("event");

    dbc->exec(sql);
}

