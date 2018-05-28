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

    WarGrey::SCADA::AlarmEvent make_event();
    void default_event(WarGrey::SCADA::AlarmEvent& self);
    void refresh_event(WarGrey::SCADA::AlarmEvent& self);
    void store_event(WarGrey::SCADA::AlarmEvent& self, WarGrey::SCADA::IPreparedStatement* stmt);
    void restore_event(WarGrey::SCADA::AlarmEvent& self, WarGrey::SCADA::IPreparedStatement* stmt);

    void create_event(WarGrey::SCADA::IDBSystem* dbc, bool if_not_exists = true);
    void insert_event(WarGrey::SCADA::IDBSystem* dbc, AlarmEvent* self, bool replace = false);
    void insert_event(WarGrey::SCADA::IDBSystem* dbc, AlarmEvent* selves, size_t count, bool replace = false);
    std::list<AlarmEvent> select_event(WarGrey::SCADA::IDBSystem* dbc, unsigned int limit = 0, unsigned int offset = 0);
    void drop_event(WarGrey::SCADA::IDBSystem* dbc);

    template<size_t N>
    void insert_event(WarGrey::SCADA::IDBSystem* dbc, AlarmEvent (&selves)[N], bool replace = false) {
        insert_event(dbc, selves, N, replace);
    }

}
