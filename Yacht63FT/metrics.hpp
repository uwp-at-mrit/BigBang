#pragma once

#include "sqlite3.hpp"
#include "schema/event.hpp"
#include "stone/tongue/logbook.hpp"

namespace WarGrey::SCADA {
	WarGrey::SCADA::SQLite3* system_logbook();

	void enter_logbook_critical_section();
	void leave_logbook_critical_section();

	void enter_logbook_shared_section();
	void leave_logbook_shared_section();
}
