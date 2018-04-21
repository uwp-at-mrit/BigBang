#pragma once

#include "syslog.hpp"

#ifdef _DEBUG
static WarGrey::SCADA::Log default_logging_level = WarGrey::SCADA::Log::Debug;
#else
static WarGrey::SCADA::Log default_logging_level = WarGrey::SCADA::Log::Info;
#endif

static Platform::String^ remote_host_at_Factory = "192.168.0.64";
static Platform::String^ remote_host_at_05 = "192.168.1.133";
static Platform::String^ remote_host_at_19 = "192.168.8.105";

static Platform::String^ remote_test_server = remote_host_at_19;
