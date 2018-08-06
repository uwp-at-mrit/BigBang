#pragma once

#include "syslog.hpp"

#ifdef _DEBUG
static WarGrey::SCADA::Log default_logging_level = WarGrey::SCADA::Log::Debug;
#else
static WarGrey::SCADA::Log default_logging_level = WarGrey::SCADA::Log::Info;
#endif

// static Platform::String^ remote_test_server = "172.20.10.2";
static Platform::String^ remote_test_server = "192.168.1.255";

/*************************************************************************************************/
static unsigned int frame_per_second = 4;
