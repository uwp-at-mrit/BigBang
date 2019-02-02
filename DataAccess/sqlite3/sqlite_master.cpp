#include "sqlite_master.hpp"

#include "dbsystem.hpp"
#include "dbtypes.hpp"

using namespace WarGrey::SCADA;

static const char* sqlite_master_rowids[] = { "rowid" };

static TableColumnInfo sqlite_master_columns[] = {
    { "rowid", SDT::Integer, nullptr, DB_PRIMARY_KEY | DB_NOT_NULL | 0 },
    { "type", SDT::Text, nullptr, 0 | DB_NOT_NULL | 0 },
    { "name", SDT::Text, nullptr, 0 | DB_NOT_NULL | 0 },
    { "tbl_name", SDT::Text, nullptr, 0 | DB_NOT_NULL | 0 },
    { "rootpage", SDT::Integer, nullptr, 0 | DB_NOT_NULL | 0 },
    { "sql", SDT::Text, nullptr, 0 | 0 | 0 },
};

/**************************************************************************************************/
SQLiteMaster_pk WarGrey::SCADA::sqlite_master_identity(SQLiteMaster& self) {
    return self.rowid;
}

SQLiteMaster WarGrey::SCADA::make_sqlite_master(std::optional<Integer> rowid, std::optional<Text> type, std::optional<Text> name, std::optional<Text> tbl_name, std::optional<Integer> rootpage, std::optional<Text> sql) {
    SQLiteMaster self;

    default_sqlite_master(self, rowid, type, name, tbl_name, rootpage, sql);

    return self;
}

void WarGrey::SCADA::default_sqlite_master(SQLiteMaster& self, std::optional<Integer> rowid, std::optional<Text> type, std::optional<Text> name, std::optional<Text> tbl_name, std::optional<Integer> rootpage, std::optional<Text> sql) {
    if (rowid.has_value()) { self.rowid = rowid.value(); }
    if (type.has_value()) { self.type = type.value(); }
    if (name.has_value()) { self.name = name.value(); }
    if (tbl_name.has_value()) { self.tbl_name = tbl_name.value(); }
    if (rootpage.has_value()) { self.rootpage = rootpage.value(); }
    if (sql.has_value()) { self.sql = sql.value(); }
}

void WarGrey::SCADA::refresh_sqlite_master(SQLiteMaster& self) {
}

void WarGrey::SCADA::store_sqlite_master(SQLiteMaster& self, IPreparedStatement* stmt) {
    stmt->bind_parameter(0U, self.rowid);
    stmt->bind_parameter(1U, self.type);
    stmt->bind_parameter(2U, self.name);
    stmt->bind_parameter(3U, self.tbl_name);
    stmt->bind_parameter(4U, self.rootpage);
    stmt->bind_parameter(5U, self.sql);
}

void WarGrey::SCADA::restore_sqlite_master(SQLiteMaster& self, IPreparedStatement* stmt) {
    self.rowid = stmt->column_int64(0U);
    self.type = stmt->column_text(1U);
    self.name = stmt->column_text(2U);
    self.tbl_name = stmt->column_text(3U);
    self.rootpage = stmt->column_int64(4U);
    self.sql = stmt->column_maybe_text(5U);
}

/**************************************************************************************************/
void WarGrey::SCADA::create_sqlite_master(IDBSystem* dbc, bool if_not_exists) {
    IVirtualSQL* vsql = dbc->make_sql_factory(sqlite_master_columns);
    std::string sql = vsql->create_table("sqlite_master", sqlite_master_rowids, sizeof(sqlite_master_rowids)/sizeof(char*), if_not_exists);

    dbc->exec(sql);
}

void WarGrey::SCADA::insert_sqlite_master(IDBSystem* dbc, SQLiteMaster& self, bool replace) {
    insert_sqlite_master(dbc, &self, 1, replace);
}

void WarGrey::SCADA::insert_sqlite_master(IDBSystem* dbc, SQLiteMaster* selves, size_t count, bool replace) {
    IVirtualSQL* vsql = dbc->make_sql_factory(sqlite_master_columns);
    std::string sql = vsql->insert_into("sqlite_master", replace);
    IPreparedStatement* stmt = dbc->prepare(sql);

    if (stmt != nullptr) {
        for (size_t i = 0; i < count; i ++) {
            store_sqlite_master(selves[i], stmt);

            dbc->exec(stmt);
            stmt->reset(true);
        }

        delete stmt;
    }
}

void WarGrey::SCADA::foreach_sqlite_master(IDBSystem* dbc, ISQLiteMasterCursor* cursor, uint64 limit, uint64 offset, sqlite_master order_by, bool asc) {
    IVirtualSQL* vsql = dbc->make_sql_factory(sqlite_master_columns);
    const char* colname = ((order_by == sqlite_master::_) ? nullptr : sqlite_master_columns[static_cast<unsigned int>(order_by)].name);
    std::string sql = vsql->select_from("sqlite_master", colname, asc, limit, offset);
    IPreparedStatement* stmt = dbc->prepare(sql);

    if (stmt != nullptr) {
        SQLiteMaster self;

        while(stmt->step()) {
            restore_sqlite_master(self, stmt);
            if (!cursor->step(self, asc, dbc->last_errno())) break;
        }

        delete stmt;
    }

}

std::list<SQLiteMaster> WarGrey::SCADA::select_sqlite_master(IDBSystem* dbc, uint64 limit, uint64 offset, sqlite_master order_by, bool asc) {
    IVirtualSQL* vsql = dbc->make_sql_factory(sqlite_master_columns);
    const char* colname = ((order_by == sqlite_master::_) ? nullptr : sqlite_master_columns[static_cast<unsigned int>(order_by)].name);
    std::string sql = vsql->select_from("sqlite_master", colname, asc, limit, offset);
    IPreparedStatement* stmt = dbc->prepare(sql);
    std::list<SQLiteMaster> queries;

    if (stmt != nullptr) {
        SQLiteMaster self;

        while(stmt->step()) {
            restore_sqlite_master(self, stmt);
            queries.push_back(self);
        }

        delete stmt;
    }

    return queries;
}

std::optional<SQLiteMaster> WarGrey::SCADA::seek_sqlite_master(IDBSystem* dbc, SQLiteMaster_pk where) {
    IVirtualSQL* vsql = dbc->make_sql_factory(sqlite_master_columns);
    std::string sql = vsql->seek_from("sqlite_master", sqlite_master_rowids, sizeof(sqlite_master_rowids)/sizeof(char*));
    IPreparedStatement* stmt = dbc->prepare(sql);
    std::optional<SQLiteMaster> query;

    if (stmt != nullptr) {
        SQLiteMaster self;

        stmt->bind_parameter(0U, where);

        if (stmt->step()) {
            restore_sqlite_master(self, stmt);
            query = self;
        }

        delete stmt;
    }

    return query;
}

void WarGrey::SCADA::update_sqlite_master(IDBSystem* dbc, SQLiteMaster& self, bool refresh) {
    update_sqlite_master(dbc, &self, 1, refresh);
}

void WarGrey::SCADA::update_sqlite_master(IDBSystem* dbc, SQLiteMaster* selves, size_t count, bool refresh) {
    IVirtualSQL* vsql = dbc->make_sql_factory(sqlite_master_columns);
    std::string sql = vsql->update_set("sqlite_master", sqlite_master_rowids, sizeof(sqlite_master_rowids)/sizeof(char*));
    IPreparedStatement* stmt = dbc->prepare(sql);

    if (stmt != nullptr) {
        for (size_t i = 0; i < count; i ++) {
            if (refresh) {
                refresh_sqlite_master(selves[i]);
            }

            stmt->bind_parameter(5U, selves[i].rowid);

            stmt->bind_parameter(0U, selves[i].type);
            stmt->bind_parameter(1U, selves[i].name);
            stmt->bind_parameter(2U, selves[i].tbl_name);
            stmt->bind_parameter(3U, selves[i].rootpage);
            stmt->bind_parameter(4U, selves[i].sql);

            dbc->exec(stmt);
            stmt->reset(true);
        }

        delete stmt;
    }
}

void WarGrey::SCADA::delete_sqlite_master(IDBSystem* dbc, SQLiteMaster_pk& where) {
    delete_sqlite_master(dbc, &where, 1);
}

void WarGrey::SCADA::delete_sqlite_master(IDBSystem* dbc, SQLiteMaster_pk* wheres, size_t count) {
    IVirtualSQL* vsql = dbc->make_sql_factory(sqlite_master_columns);
    std::string sql = vsql->delete_from("sqlite_master", sqlite_master_rowids, sizeof(sqlite_master_rowids)/sizeof(char*));
    IPreparedStatement* stmt = dbc->prepare(sql);

    if (stmt != nullptr) {
        for (size_t i = 0; i < count; i ++) {
            stmt->bind_parameter(0U, wheres[i]);

            dbc->exec(stmt);
            stmt->reset(true);
        }

        delete stmt;
    }
}

void WarGrey::SCADA::drop_sqlite_master(IDBSystem* dbc) {
    IVirtualSQL* vsql = dbc->make_sql_factory(sqlite_master_columns);
    std::string sql = vsql->drop_table("sqlite_master");

    dbc->exec(sql);
}

/**************************************************************************************************/
double WarGrey::SCADA::sqlite_master_average(WarGrey::SCADA::IDBSystem* dbc, sqlite_master column, bool distinct) {
    IVirtualSQL* vsql = dbc->make_sql_factory(sqlite_master_columns);
    const char* colname = ((column == sqlite_master::_) ? nullptr : sqlite_master_columns[static_cast<unsigned int>(column)].name);

    return dbc->query_double(vsql->table_average("sqlite_master", colname, distinct));
}

int64 WarGrey::SCADA::sqlite_master_count(WarGrey::SCADA::IDBSystem* dbc, sqlite_master column, bool distinct) {
    IVirtualSQL* vsql = dbc->make_sql_factory(sqlite_master_columns);
    const char* colname = ((column == sqlite_master::_) ? nullptr : sqlite_master_columns[static_cast<unsigned int>(column)].name);

    return dbc->query_int64(vsql->table_count("sqlite_master", colname, distinct));
}

std::optional<double> WarGrey::SCADA::sqlite_master_max(WarGrey::SCADA::IDBSystem* dbc, sqlite_master column, bool distinct) {
    IVirtualSQL* vsql = dbc->make_sql_factory(sqlite_master_columns);
    const char* colname = ((column == sqlite_master::_) ? nullptr : sqlite_master_columns[static_cast<unsigned int>(column)].name);

    return dbc->query_maybe_double(vsql->table_max("sqlite_master", colname, distinct));
}

std::optional<double> WarGrey::SCADA::sqlite_master_min(WarGrey::SCADA::IDBSystem* dbc, sqlite_master column, bool distinct) {
    IVirtualSQL* vsql = dbc->make_sql_factory(sqlite_master_columns);
    const char* colname = ((column == sqlite_master::_) ? nullptr : sqlite_master_columns[static_cast<unsigned int>(column)].name);

    return dbc->query_maybe_double(vsql->table_min("sqlite_master", colname, distinct));
}

std::optional<double> WarGrey::SCADA::sqlite_master_sum(WarGrey::SCADA::IDBSystem* dbc, sqlite_master column, bool distinct) {
    IVirtualSQL* vsql = dbc->make_sql_factory(sqlite_master_columns);
    const char* colname = ((column == sqlite_master::_) ? nullptr : sqlite_master_columns[static_cast<unsigned int>(column)].name);

    return dbc->query_maybe_double(vsql->table_sum("sqlite_master", colname, distinct));
}

