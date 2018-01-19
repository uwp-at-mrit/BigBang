#pragma once

#define _USE_MATH_DEFINES
#include <WindowsNumerics.h>

#include "forward.hpp"

Windows::Foundation::Rect snip_enclosing_box(
    WarGrey::SCADA::ISnip* snip, float x, float y,
    Windows::Foundation::Numerics::float3x2 tf);

void snip_save(WarGrey::SCADA::ISnip* snip, Platform::String^ path, float dpi = 96.0F);
