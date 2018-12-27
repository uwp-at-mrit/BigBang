#pragma once

#include "satellite.hpp"

namespace WarGrey::SCADA {
	WarGrey::SCADA::ISatellite* the_gallery();
	void update_the_shown_gallery(long long count, long long interval, long long uptime, bool create = false);
}
