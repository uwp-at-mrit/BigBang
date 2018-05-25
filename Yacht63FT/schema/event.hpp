#pragma once

#include <optional>

#include "dbsystem.hpp"

namespace WarGrey::SCADA {
    private struct AlarmEvent {
        Integer uuid;
        Text type;
        Text name;
        std::optional<Integer> ctime;
        std::optional<Integer> mtime;
    };

    void create_event(WarGrey::SCADA::IDBSystem* dbc, bool if_not_exists = true);
    void drop_event(WarGrey::SCADA::IDBSystem* dbc);
}
