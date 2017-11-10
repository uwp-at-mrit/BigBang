#pragma once

#define _USE_MATH_DEFINES
#include <WindowsNumerics.h>

#include "forward.hpp"

Windows::Foundation::Rect snip_enclosing_box(
    WarGrey::SCADA::Snip* snip, float x, float y,
    Windows::Foundation::Numerics::float3x2 tf);

Windows::Foundation::Rect snip_socket_hflip(
    WarGrey::SCADA::Snip* snip, float x, float y,
    Windows::Foundation::Rect& socket);

void snip_draw_with_hflipping(WarGrey::SCADA::Snip* snip,
    Microsoft::Graphics::Canvas::CanvasDrawingSession^ ds,
    float x, float y, float Width, float Height);
