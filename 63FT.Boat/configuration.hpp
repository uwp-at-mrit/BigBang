#pragma once

#include "syslog.hpp"

#ifdef Release
static WarGrey::SCADA::Log default_logging_level = WarGrey::SCADA::Log::Info;
#else
static WarGrey::SCADA::Log default_logging_level = WarGrey::SCADA::Log::Debug;
#endif
