#include "snip/misc.hpp"
#include "snip.hpp"
#include "shape.hpp"

using namespace WarGrey::SCADA;

using namespace Windows::Foundation;
using namespace Windows::Foundation::Numerics;

Rect snip_enclosing_box(ISnip* snip, float x, float y, float3x2 transform) {
    float width, height;

    snip->fill_extent(x, y, &width, &height);
    
    return rectangle(x, y, width, height)->ComputeBounds(transform);
}
