#include "snip/pipe/pipesnip.hpp"

using namespace WarGrey::SCADA;

using namespace Windows::Foundation;
using namespace Windows::Foundation::Numerics;

/*************************************************************************************************/
void WarGrey::SCADA::pipe_connecting_position(IPipeSnip* prev, IPipeSnip* pipe, float* x, float* y) {
    Rect in = pipe->get_input_port();
    Rect out = prev->get_output_port();

    float delta_x = (out.X - in.X) - (out.Width - in.Width) * 0.5F;
    float delta_y = (out.Y - in.Y) - (out.Height - in.Height) * 0.5F;

    if (x != nullptr) { (*x) = (*x) + delta_x; }
    if (y != nullptr) { (*y) = (*y) + delta_y; }
}

float2 WarGrey::SCADA::pipe_connecting_position(IPipeSnip* prev, IPipeSnip* pipe, float x0, float y0) {
    float x = x0;
    float y = y0;
    
    pipe_connecting_position(prev, pipe, &x, &y);

    return float2{ x, y};
}
