#pragma once

#include "syslog.hpp"

#ifdef _DEBUG
static WarGrey::SCADA::Log default_logging_level = WarGrey::SCADA::Log::Debug;
#else
static WarGrey::SCADA::Log default_logging_level = WarGrey::SCADA::Log::Info;
#endif

static Platform::String^ remote_host_at_05 = "192.168.1.133";
static Platform::String^ remote_host_at_19 = "192.168.8.100";

static Platform::String^ remote_test_server = remote_host_at_19;
