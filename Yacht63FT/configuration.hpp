#pragma once

#include "syslog.hpp"

#ifdef _DEBUG
static WarGrey::SCADA::Log default_logging_level = WarGrey::SCADA::Log::Debug;
#else
static WarGrey::SCADA::Log default_logging_level = WarGrey::SCADA::Log::Info;
#endif

static Platform::String^ remote_host_at_19 = "192.168.8.106";

static Platform::String^ remote_test_server = remote_host_at_19;

/*************************************************************************************************/
static float resolution_width = 1920.0F;
static float resolution_height = 1080.0F;
static float resolution_navigator_height = 90.0F;
static float resolution_statusbar_height = 215.0F;

float application_fit_size(float src);
