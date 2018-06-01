#pragma once

#include <list>
#include <optional>

#include "dbsystem.hpp"

namespace WarGrey::SCADA {
    typedef Integer AlarmEvent_pk;

    private struct AlarmEvent {
        Integer uuid;
        Text type;
        Text name;
        std::optional<Integer> ctime;
        std::optional<Integer> mtime;
    };

    private enum class event { uuid, type, name, ctime, mtime, _ };

    WarGrey::SCADA::AlarmEvent_pk event_identity(WarGrey::SCADA::AlarmEvent& self);

    WarGrey::SCADA::AlarmEvent make_event(std::optional<Text> type = std::nullopt, std::optional<Text> name = std::nullopt);
    void default_event(WarGrey::SCADA::AlarmEvent& self, std::optional<Text> type = std::nullopt, std::optional<Text> name = std::nullopt);
    void refresh_event(WarGrey::SCADA::AlarmEvent& self);
    void store_event(WarGrey::SCADA::AlarmEvent& self, WarGrey::SCADA::IPreparedStatement* stmt);
    void restore_event(WarGrey::SCADA::AlarmEvent& self, WarGrey::SCADA::IPreparedStatement* stmt);

    void create_event(WarGrey::SCADA::IDBSystem* dbc, bool if_not_exists = true);
    void insert_event(WarGrey::SCADA::IDBSystem* dbc, AlarmEvent& self, bool replace = false);
    void insert_event(WarGrey::SCADA::IDBSystem* dbc, AlarmEvent* selves, size_t count, bool replace = false);
    std::list<WarGrey::SCADA::AlarmEvent_pk> list_event(WarGrey::SCADA::IDBSystem* dbc, uint64 limit = 0, uint64 offset = 0, const char* order_by = "ctime");
    std::list<WarGrey::SCADA::AlarmEvent> select_event(WarGrey::SCADA::IDBSystem* dbc, uint64 limit = 0, uint64 offset = 0, const char* order_by = "ctime");
    std::optional<WarGrey::SCADA::AlarmEvent> seek_event(WarGrey::SCADA::IDBSystem* dbc, WarGrey::SCADA::AlarmEvent_pk where);
    void update_event(WarGrey::SCADA::IDBSystem* dbc, WarGrey::SCADA::AlarmEvent& self, bool refresh = true);
    void update_event(WarGrey::SCADA::IDBSystem* dbc, WarGrey::SCADA::AlarmEvent* selves, size_t count, bool refresh = true);
    void delete_event(WarGrey::SCADA::IDBSystem* dbc, WarGrey::SCADA::AlarmEvent_pk& where);
    void delete_event(WarGrey::SCADA::IDBSystem* dbc, WarGrey::SCADA::AlarmEvent_pk* wheres, size_t count);
    void drop_event(WarGrey::SCADA::IDBSystem* dbc);

    double event_average(WarGrey::SCADA::IDBSystem* dbc, WarGrey::SCADA::event column = WarGrey::SCADA::event::_, bool distinct = false);
    int64 event_count(WarGrey::SCADA::IDBSystem* dbc, WarGrey::SCADA::event column = WarGrey::SCADA::event::_, bool distinct = false);
    std::optional<double> event_max(WarGrey::SCADA::IDBSystem* dbc, WarGrey::SCADA::event column = WarGrey::SCADA::event::_, bool distinct = false);
    std::optional<double> event_min(WarGrey::SCADA::IDBSystem* dbc, WarGrey::SCADA::event column = WarGrey::SCADA::event::_, bool distinct = false);
    std::optional<double> event_sum(WarGrey::SCADA::IDBSystem* dbc, WarGrey::SCADA::event column = WarGrey::SCADA::event::_, bool distinct = false);

    template<size_t N>
    void insert_event(WarGrey::SCADA::IDBSystem* dbc, AlarmEvent (&selves)[N], bool replace = false) {
        WarGrey::SCADA::insert_event(dbc, selves, N, replace);
    }

    template<size_t N>
    void update_event(WarGrey::SCADA::IDBSystem* dbc, AlarmEvent (&selves)[N], bool refresh = true) {
        WarGrey::SCADA::update_event(dbc, selves, N, refresh);
    }

    template<size_t N>
    void delete_event(WarGrey::SCADA::IDBSystem* dbc, AlarmEvent_pk (&wheres)[N]) {
        WarGrey::SCADA::delete_event(dbc, wheres, N);
    }

}
