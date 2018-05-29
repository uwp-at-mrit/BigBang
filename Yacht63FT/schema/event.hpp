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

    WarGrey::SCADA::AlarmEvent_pk event_identity(WarGrey::SCADA::AlarmEvent& self);

    WarGrey::SCADA::AlarmEvent make_event(std::optional<Text> type = std::nullopt, std::optional<Text> name = std::nullopt);
    void default_event(WarGrey::SCADA::AlarmEvent& self, std::optional<Text> type = std::nullopt, std::optional<Text> name = std::nullopt);
    void refresh_event(WarGrey::SCADA::AlarmEvent& self);
    void store_event(WarGrey::SCADA::AlarmEvent& self, WarGrey::SCADA::IPreparedStatement* stmt);
    void restore_event(WarGrey::SCADA::AlarmEvent& self, WarGrey::SCADA::IPreparedStatement* stmt);

    void create_event(WarGrey::SCADA::IDBSystem* dbc, bool if_not_exists = true);
    void insert_event(WarGrey::SCADA::IDBSystem* dbc, AlarmEvent& self, bool replace = false);
    void insert_event(WarGrey::SCADA::IDBSystem* dbc, AlarmEvent* selves, size_t count, bool replace = false);
    std::list<WarGrey::SCADA::AlarmEvent_pk> list_event(WarGrey::SCADA::IDBSystem* dbc, unsigned int limit = 0, unsigned int offset = 0);
    std::list<WarGrey::SCADA::AlarmEvent> select_event(WarGrey::SCADA::IDBSystem* dbc, unsigned int limit = 0, unsigned int offset = 0);
    std::optional<WarGrey::SCADA::AlarmEvent> seek_event(WarGrey::SCADA::IDBSystem* dbc, WarGrey::SCADA::AlarmEvent_pk where);
    void delete_event(WarGrey::SCADA::IDBSystem* dbc, WarGrey::SCADA::AlarmEvent_pk& where);
    void delete_event(WarGrey::SCADA::IDBSystem* dbc, WarGrey::SCADA::AlarmEvent_pk* where, size_t count);
    void drop_event(WarGrey::SCADA::IDBSystem* dbc);

    template<size_t N>
    void insert_event(WarGrey::SCADA::IDBSystem* dbc, AlarmEvent (&selves)[N], bool replace = false) {
        WarGrey::SCADA::insert_event(dbc, selves, N, replace);
    }

    template<size_t N>
    void delete_event(WarGrey::SCADA::IDBSystem* dbc, AlarmEvent_pk (&wheres)[N]) {
        WarGrey::SCADA::delete_event(dbc, wheres, N);
    }

}
