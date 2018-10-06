#pragma once

#include "syslog.hpp"

#ifdef _DEBUG
static WarGrey::SCADA::Log default_logging_level = WarGrey::SCADA::Log::Debug;
#else
static WarGrey::SCADA::Log default_logging_level = WarGrey::SCADA::Log::Info;
#endif

static Platform::String^ remote_test_server = "192.168.0.8";

/*************************************************************************************************/
static const unsigned int frame_per_second = 4;

static const size_t hopper_count = 7;

static const float metrics_font_size = 20.0F;
static const float large_font_size  = 18.0F;
static const float normal_font_size = 16.0F;
static const float small_font_size  = 14.0F;
static const float tiny_font_size   = 12.0F;

static const unsigned int default_ps_color = 0xFF0000;
static const unsigned int default_sb_color = 0x00FF00;
static const unsigned int default_pipe_color = 0xC0C0C0;

static const float default_pipe_thickness = 1.5F;

/*************************************************************************************************/
#define resolve_gridsize(gwidth, gheight) ((gwidth < gheight) ? gheight : gwidth)
