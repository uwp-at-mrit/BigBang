#include "event.hpp"

#include "dbsystem.hpp"
#include "dbtypes.hpp"

#include "dbmisc.hpp"

using namespace WarGrey::SCADA;

static const char* event_rowids[] = { "uuid", "name" };

static TableColumnInfo event_columns[] = {
    { "uuid", SDT::Integer, nullptr, DB_PRIMARY_KEY | 0 | 0 },
    { "type", SDT::Text, nullptr, 0 | DB_NOT_NULL | 0 },
    { "name", SDT::Text, nullptr, DB_PRIMARY_KEY | DB_NOT_NULL | DB_UNIQUE },
    { "ctime", SDT::Integer, nullptr, 0 | 0 | 0 },
    { "mtime", SDT::Integer, nullptr, 0 | 0 | 0 },
};

/**************************************************************************************************/
AlarmEvent_pk WarGrey::SCADA::event_identity(AlarmEvent& self) {
    return { self.uuid, self.name };
}

AlarmEvent WarGrey::SCADA::make_event(std::optional<Text> type, std::optional<Text> name) {
    AlarmEvent self;

    default_event(self, type, name);

    return self;
}

void WarGrey::SCADA::default_event(AlarmEvent& self, std::optional<Text> type, std::optional<Text> name) {
    self.uuid = pk64_timestamp();
    self.type = ((type.has_value()) ? type.value() : "table");
    if (name.has_value()) { self.name = name.value(); }
    self.ctime = current_milliseconds();
    self.mtime = current_milliseconds();
}

void WarGrey::SCADA::refresh_event(AlarmEvent& self) {
    self.mtime = current_milliseconds();
}

void WarGrey::SCADA::store_event(AlarmEvent& self, IPreparedStatement* stmt) {
    stmt->bind_parameter(0U, self.uuid);
    stmt->bind_parameter(1U, self.type);
    stmt->bind_parameter(2U, self.name);
    stmt->bind_parameter(3U, self.ctime);
    stmt->bind_parameter(4U, self.mtime);
}

void WarGrey::SCADA::restore_event(AlarmEvent& self, IPreparedStatement* stmt) {
    self.uuid = stmt->column_int64(0U);
    self.type = stmt->column_text(1U);
    self.name = stmt->column_text(2U);
    self.ctime = stmt->column_maybe_int64(3U);
    self.mtime = stmt->column_maybe_int64(4U);
}

/**************************************************************************************************/
void WarGrey::SCADA::create_event(IDBSystem* dbc, bool if_not_exists) {
    IVirtualSQL* vsql = dbc->make_sql_factory(event_columns);
    std::string sql = vsql->create_table("event", event_rowids, if_not_exists);

    dbc->exec(sql);
}

void WarGrey::SCADA::insert_event(IDBSystem* dbc, AlarmEvent& self, bool replace) {
    insert_event(dbc, &self, 1, replace);
}

void WarGrey::SCADA::insert_event(IDBSystem* dbc, AlarmEvent* selves, size_t count, bool replace) {
    IVirtualSQL* vsql = dbc->make_sql_factory(event_columns);
    std::string sql = vsql->insert_into("event", replace);
    IPreparedStatement* stmt = dbc->prepare(sql);

    if (stmt != nullptr) {
        for (int i = 0; i < count; i ++) {
            store_event(selves[i], stmt);

            dbc->exec(stmt);
            stmt->reset(true);
        }

        delete stmt;
    }
}

std::list<AlarmEvent_pk> WarGrey::SCADA::list_event(IDBSystem* dbc, unsigned int limit, unsigned int offset) {
    IVirtualSQL* vsql = dbc->make_sql_factory(event_columns);
    std::string sql = vsql->select_from("event", event_rowids, limit, offset);
    IPreparedStatement* stmt = dbc->prepare(sql);
    std::list<AlarmEvent_pk> queries;

    if (stmt != nullptr) {
        AlarmEvent_pk self;

        while(stmt->step()) {
            self.uuid = stmt->column_int64(0U);
            self.name = stmt->column_text(1U);

            queries.push_back(self);
        }

        delete stmt;
    }

    return queries;
}

std::list<AlarmEvent> WarGrey::SCADA::select_event(IDBSystem* dbc, unsigned int limit, unsigned int offset) {
    IVirtualSQL* vsql = dbc->make_sql_factory(event_columns);
    std::string sql = vsql->select_from("event", limit, offset);
    IPreparedStatement* stmt = dbc->prepare(sql);
    std::list<AlarmEvent> queries;

    if (stmt != nullptr) {
        AlarmEvent self;

        while(stmt->step()) {
            restore_event(self, stmt);
            queries.push_back(self);
        }

        delete stmt;
    }

    return queries;
}

std::optional<AlarmEvent> WarGrey::SCADA::seek_event(IDBSystem* dbc, AlarmEvent_pk where) {
    IVirtualSQL* vsql = dbc->make_sql_factory(event_columns);
    std::string sql = vsql->seek_from("event", event_rowids);
    IPreparedStatement* stmt = dbc->prepare(sql);
    std::optional<AlarmEvent> query;

    if (stmt != nullptr) {
        AlarmEvent self;

        stmt->bind_parameter(0U, where.uuid);
        stmt->bind_parameter(1U, where.name);

        if (stmt->step()) {
            restore_event(self, stmt);
            query = self;
        }

        delete stmt;
    }

    return query;
}

void WarGrey::SCADA::update_event(IDBSystem* dbc, AlarmEvent& self, bool refresh) {
    update_event(dbc, &self, 1);
}

void WarGrey::SCADA::update_event(IDBSystem* dbc, AlarmEvent* selves, size_t count, bool refresh) {
    IVirtualSQL* vsql = dbc->make_sql_factory(event_columns);
    std::string sql = vsql->update_set("event", event_rowids);
    IPreparedStatement* stmt = dbc->prepare(sql);

    if (stmt != nullptr) {
        for (int i = 0; i < count; i ++) {
            if (refresh) { refresh_event(selves[i]); }

            stmt->bind_parameter(3U, selves[i].uuid);
            stmt->bind_parameter(4U, selves[i].name);

            stmt->bind_parameter(0U, selves[i].type);
            stmt->bind_parameter(1U, selves[i].ctime);
            stmt->bind_parameter(2U, selves[i].mtime);

            dbc->exec(stmt);
            stmt->reset(true);
        }

        delete stmt;
    }
}

void WarGrey::SCADA::delete_event(IDBSystem* dbc, AlarmEvent_pk& where) {
    delete_event(dbc, &where, 1);
}

void WarGrey::SCADA::delete_event(IDBSystem* dbc, AlarmEvent_pk* wheres, size_t count) {
    IVirtualSQL* vsql = dbc->make_sql_factory(event_columns);
    std::string sql = vsql->delete_from("event", event_rowids);
    IPreparedStatement* stmt = dbc->prepare(sql);

    if (stmt != nullptr) {
        for (int i = 0; i < count; i ++) {
            stmt->bind_parameter(0U, wheres[i].uuid);
            stmt->bind_parameter(1U, wheres[i].name);

            dbc->exec(stmt);
            stmt->reset(true);
        }

        delete stmt;
    }
}

void WarGrey::SCADA::drop_event(IDBSystem* dbc) {
    IVirtualSQL* vsql = dbc->make_sql_factory(event_columns);
    std::string sql = vsql->drop_table("event");

    dbc->exec(sql);
}

