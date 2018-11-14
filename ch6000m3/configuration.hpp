#pragma once

#include "syslog.hpp"

#ifdef _DEBUG
static WarGrey::SCADA::Log default_logging_level = WarGrey::SCADA::Log::Debug;
#else
static WarGrey::SCADA::Log default_logging_level = WarGrey::SCADA::Log::Info;
#endif

//static Platform::String^ remote_test_server = "172.20.10.2";
static Platform::String^ remote_test_server = "192.168.0.123";

/*************************************************************************************************/
static const unsigned int frame_per_second = 4U;

static const float large_metrics_font_size = 24.0F;
static const float normal_metrics_font_size = 22.0F;
static const float small_metrics_font_size = 20.0F;

static const float large_font_size = 18.0F;
static const float normal_font_size = 16.0F;
static const float small_font_size = 14.0F;
static const float tiny_font_size = 12.0F;

/*************************************************************************************************/
static const size_t hopper_count = 7;

static const double hopper_height_range = 13.85;
static const double earthwork_range = 10000.0;
static const double vessel_range = 10000.0;
static const double loading_range = 14000.0;
static const double displacement_range = 30000.0;
static const double compensator_range = 4.0;

static const float ps_drag_trunnion_gapsize = 1.845F;
static const float ps_drag_trunnion_length = 3.61F;
static const float ps_drag_pipe1_length = 25.34F;
static const float ps_drag_pipe2_length = 17.55F;
static const float ps_drag_radius = 0.5F;
static const float ps_drag_head_width = 4.03F;
static const float ps_drag_head_length = 2.54F;
static const float ps_drag_head_compensation = 0.5F;

static const float sb_drag_trunnion_gapsize = 1.845F;
static const float sb_drag_trunnion_length = 3.61F;
static const float sb_drag_pipe1_length = 25.34F;
static const float sb_drag_pipe2_length = 17.45F;  // the long one is 30.55F
static const float sb_drag_radius = 0.5F;
static const float sb_drag_head_width = 4.03F;
static const float sb_drag_head_length = 2.54F;
static const float sb_drag_head_compensation = 0.5F;

static const unsigned int default_ps_color = 0xFF0000;
static const unsigned int default_sb_color = 0x008000;
static const unsigned int default_pipe_color = 0xC0C0C0;

static const float default_pipe_thickness = 2.0F;

/*************************************************************************************************/
#define resolve_gridsize(gwidth, gheight) ((gwidth < gheight) ? gheight : gwidth)
