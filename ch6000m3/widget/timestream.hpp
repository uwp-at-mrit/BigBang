#pragma once

#include "timemachine.hpp"
#include "plc.hpp"

namespace WarGrey::SCADA {
	void initialize_the_timemachine(WarGrey::SCADA::PLCMaster* plc, int frame_rate, long long snapshot_interval = 2);
	void launch_the_timemachine();
}
