#pragma once

#include <list>
#include <optional>

#include "dbsystem.hpp"

namespace WarGrey::SCADA {
    typedef Integer SQLiteMaster_pk;

    private struct SQLiteMaster {
        Integer rowid;
        Text type;
        Text name;
        Text tbl_name;
        Integer rootpage;
        std::optional<Text> sql;
    };

    private class ISQLiteMasterCursor abstract {
    public:
        virtual bool step(WarGrey::SCADA::SQLiteMaster& occurrence, bool asc, int code) = 0;
    };

    private enum class sqlite_master { rowid, type, name, tbl_name, rootpage, sql, _ };

    WarGrey::SCADA::SQLiteMaster_pk sqlite_master_identity(WarGrey::SCADA::SQLiteMaster& self);

    WarGrey::SCADA::SQLiteMaster make_sqlite_master(std::optional<Integer> rowid = std::nullopt, std::optional<Text> type = std::nullopt, std::optional<Text> name = std::nullopt, std::optional<Text> tbl_name = std::nullopt, std::optional<Integer> rootpage = std::nullopt, std::optional<Text> sql = std::nullopt);
    void default_sqlite_master(WarGrey::SCADA::SQLiteMaster& self, std::optional<Integer> rowid = std::nullopt, std::optional<Text> type = std::nullopt, std::optional<Text> name = std::nullopt, std::optional<Text> tbl_name = std::nullopt, std::optional<Integer> rootpage = std::nullopt, std::optional<Text> sql = std::nullopt);
    void refresh_sqlite_master(WarGrey::SCADA::SQLiteMaster& self);
    void store_sqlite_master(WarGrey::SCADA::SQLiteMaster& self, WarGrey::SCADA::IPreparedStatement* stmt);
    void restore_sqlite_master(WarGrey::SCADA::SQLiteMaster& self, WarGrey::SCADA::IPreparedStatement* stmt);

    void create_sqlite_master(WarGrey::SCADA::IDBSystem* dbc, bool if_not_exists = true);
    void insert_sqlite_master(WarGrey::SCADA::IDBSystem* dbc, SQLiteMaster& self, bool replace = false);
    void insert_sqlite_master(WarGrey::SCADA::IDBSystem* dbc, SQLiteMaster* selves, size_t count, bool replace = false);
    void foreach_sqlite_master(WarGrey::SCADA::IDBSystem* dbc, WarGrey::SCADA::ISQLiteMasterCursor* cursor, uint64 limit = 0U, uint64 offset = 0U, WarGrey::SCADA::sqlite_master order_by = sqlite_master::rowid, bool asc = true);
    std::list<WarGrey::SCADA::SQLiteMaster> select_sqlite_master(WarGrey::SCADA::IDBSystem* dbc, uint64 limit = 0U, uint64 offset = 0U, WarGrey::SCADA::sqlite_master order_by = sqlite_master::rowid, bool asc = true);
    std::optional<WarGrey::SCADA::SQLiteMaster> seek_sqlite_master(WarGrey::SCADA::IDBSystem* dbc, WarGrey::SCADA::SQLiteMaster_pk where);
    void update_sqlite_master(WarGrey::SCADA::IDBSystem* dbc, WarGrey::SCADA::SQLiteMaster& self, bool refresh = true);
    void update_sqlite_master(WarGrey::SCADA::IDBSystem* dbc, WarGrey::SCADA::SQLiteMaster* selves, size_t count, bool refresh = true);
    void delete_sqlite_master(WarGrey::SCADA::IDBSystem* dbc, WarGrey::SCADA::SQLiteMaster_pk& where);
    void delete_sqlite_master(WarGrey::SCADA::IDBSystem* dbc, WarGrey::SCADA::SQLiteMaster_pk* wheres, size_t count);
    void drop_sqlite_master(WarGrey::SCADA::IDBSystem* dbc);

    double sqlite_master_average(WarGrey::SCADA::IDBSystem* dbc, WarGrey::SCADA::sqlite_master column = sqlite_master::_, bool distinct = false);
    int64 sqlite_master_count(WarGrey::SCADA::IDBSystem* dbc, WarGrey::SCADA::sqlite_master column = sqlite_master::_, bool distinct = false);
    std::optional<double> sqlite_master_max(WarGrey::SCADA::IDBSystem* dbc, WarGrey::SCADA::sqlite_master column = sqlite_master::_, bool distinct = false);
    std::optional<double> sqlite_master_min(WarGrey::SCADA::IDBSystem* dbc, WarGrey::SCADA::sqlite_master column = sqlite_master::_, bool distinct = false);
    std::optional<double> sqlite_master_sum(WarGrey::SCADA::IDBSystem* dbc, WarGrey::SCADA::sqlite_master column = sqlite_master::_, bool distinct = false);

    template<size_t N>
    void insert_sqlite_master(WarGrey::SCADA::IDBSystem* dbc, SQLiteMaster (&selves)[N], bool replace = false) {
        WarGrey::SCADA::insert_sqlite_master(dbc, selves, N, replace);
    }

    template<size_t N>
    void update_sqlite_master(WarGrey::SCADA::IDBSystem* dbc, SQLiteMaster (&selves)[N], bool refresh = true) {
        WarGrey::SCADA::update_sqlite_master(dbc, selves, N, refresh);
    }

    template<size_t N>
    void delete_sqlite_master(WarGrey::SCADA::IDBSystem* dbc, SQLiteMaster_pk (&wheres)[N]) {
        WarGrey::SCADA::delete_sqlite_master(dbc, wheres, N);
    }

}
