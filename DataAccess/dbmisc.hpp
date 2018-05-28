#pragma once

#include <cinttypes>

namespace WarGrey::SCADA {
	int64 current_milliseconds();
	int64 current_microseconds();

	int64 pk64_timestamp();
	int64 pk64_random();
}
