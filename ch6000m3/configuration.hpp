#pragma once

#include "syslog.hpp"

#ifdef _DEBUG
static WarGrey::SCADA::Log default_logging_level = WarGrey::SCADA::Log::Debug;
#else
static WarGrey::SCADA::Log default_logging_level = WarGrey::SCADA::Log::Info;
#endif

static Platform::String^ remote_host_at_05 = "192.168.0.80";
static Platform::String^ remote_host_at_04 = "192.168.2.166";
static Platform::String^ remote_host_at_19 = "192.168.8.102";
static Platform::String^ remote_test_server = remote_host_at_19;
