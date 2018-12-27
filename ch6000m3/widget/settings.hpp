#pragma once

#include "satellite.hpp"

namespace WarGrey::SCADA {
	WarGrey::SCADA::ISatellite* make_settings(WarGrey::SCADA::PLCMaster* device);
}
