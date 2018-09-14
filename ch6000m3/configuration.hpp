#pragma once

#include "syslog.hpp"

#ifdef _DEBUG
static WarGrey::SCADA::Log default_logging_level = WarGrey::SCADA::Log::Debug;
#else
static WarGrey::SCADA::Log default_logging_level = WarGrey::SCADA::Log::Info;
#endif

// static Platform::String^ remote_test_server = "172.20.10.2";
static Platform::String^ remote_test_server = "192.168.1.255";
// static Platform::String^ remote_test_server = "192.168.0.255";

/*************************************************************************************************/
static const unsigned int frame_per_second = 4;

static const size_t door_count_per_side = 7;

static const float large_font_size = 18.0F;

static const unsigned int default_port_color = 0xFF0000;
static const unsigned int default_starboard_color = 0x00FF00;
static const unsigned int default_pipeline_color = 0xC0C0C0;

static const float default_pipeline_thickness = 1.5F;
