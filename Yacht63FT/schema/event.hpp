#pragma once

#include "dbsystem.hpp"

namespace WarGrey::SCADA {
    private struct AlarmEvent {
        Integer uuid;
        Text type;
        Text name;
        Integer ctime;
        Integer mtime;
    };

    void create_event(WarGrey::SCADA::IDBSystem* dbc, bool if_not_exists = true, Platform::String^ tablename = "event");
}
