#pragma once

#include "syslog.hpp"

#ifdef Release
static WarGrey::SCADA::Log default_logging_level = WarGrey::SCADA::Log::Info;
#else
static WarGrey::SCADA::Log default_logging_level = WarGrey::SCADA::Log::Debug;
#endif

static Platform::String^ remote_host_at_05 = "192.168.1.135";
static Platform::String^ remote_host_at_04 = "192.168.2.166";
static Platform::String^ remote_host_at_19 = "192.168.8.114";
static Platform::String^ remote_test_server = remote_host_at_05;
