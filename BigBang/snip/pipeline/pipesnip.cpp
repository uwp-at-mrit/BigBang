#include "snip/pipeline/pipesnip.hpp"

using namespace WarGrey::SCADA;

using namespace Windows::Foundation;
using namespace Windows::Foundation::Numerics;

void WarGrey::SCADA::pipe_connecting_position(IPipeSnip* prev, IPipeSnip* pipe, float* x, float* y, double fx, double fy) {
    Rect in = pipe->get_inlet();
    Rect out = prev->get_outlet();

    float delta_x = (out.X - in.X) + (out.Width - in.Width) * float(fx);
    float delta_y = (out.Y - in.Y) + (out.Height - in.Height) * float(fy);

    if (x != nullptr) { (*x) = (*x) + delta_x; }
    if (y != nullptr) { (*y) = (*y) + delta_y; }
}

float2 WarGrey::SCADA::pipe_connecting_position(IPipeSnip* prev, IPipeSnip* pipe, float x0, float y0, double fx, double fy) {
    float x = x0;
    float y = y0;
    
    pipe_connecting_position(prev, pipe, &x, &y, fx, fy);

    return float2{ x, y};
}
