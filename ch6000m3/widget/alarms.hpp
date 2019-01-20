#pragma once

#include "plc.hpp"

namespace WarGrey::SCADA {
	void initialize_the_alarm(WarGrey::SCADA::PLCMaster* plc);
	void display_the_alarm();
}
