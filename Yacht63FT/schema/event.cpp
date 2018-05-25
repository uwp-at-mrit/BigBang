#include "event.hpp"

#include "dbsystem.hpp"
#include "dbtypes.hpp"

using namespace WarGrey::SCADA;

static TableColumnInfo event_columns[] = {
    { "uuid", SDT::Integer, nullptr, DB_PRIMARY_KEY | 0 | 0 },
    { "type", SDT::Text, nullptr, 0 | DB_NOT_NULL | 0 },
    { "name", SDT::Text, nullptr, DB_PRIMARY_KEY | DB_NOT_NULL | DB_UNIQUE },
    { "ctime", SDT::Integer, nullptr, 0 | 0 | 0 },
    { "mtime", SDT::Integer, nullptr, 0 | 0 | 0 },
};

void WarGrey::SCADA::create_event(IDBSystem* dbc, bool if_not_exists, Platform::String^ tablename) {
    Platform::String^ rowids[] = { "uuid", "name" };
    IVirtualSQL* vsql = dbc->make_sql_factory(event_columns, sizeof(event_columns)/sizeof(TableColumnInfo));
    Platform::String^ sql = vsql->create_table(tablename, rowids, sizeof(rowids)/sizeof(Platform::String^), if_not_exists);

    dbc->exec(sql);
}

