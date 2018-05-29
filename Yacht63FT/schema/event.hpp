#pragma once

#include <list>
#include <optional>

#include "dbsystem.hpp"

namespace WarGrey::SCADA {
    private struct AlarmEvent_pk {
        Integer uuid;
        Text name;
    };

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
    std::list<AlarmEvent> select_event(WarGrey::SCADA::IDBSystem* dbc, unsigned int limit = 0, unsigned int offset = 0);
    std::optional<AlarmEvent> seek_event(WarGrey::SCADA::IDBSystem* dbc, WarGrey::SCADA::AlarmEvent_pk& where);
    void drop_event(WarGrey::SCADA::IDBSystem* dbc);

    template<size_t N>
    void insert_event(WarGrey::SCADA::IDBSystem* dbc, AlarmEvent (&selves)[N], bool replace = false) {
        insert_event(dbc, selves, N, replace);
    }

}
