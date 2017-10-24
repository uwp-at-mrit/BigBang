#include "snip.hpp"
#include "geometry.hpp"

using namespace WarGrey::SCADA;
using namespace Windows::Foundation;
using namespace Windows::Foundation::Numerics;
using namespace Microsoft::Graphics::Canvas::Geometry;

Rect WarGrey::SCADA::snip_enclosing_box(Snip* snip, float x, float y, float3x2 transform) {
    float width, height;

    snip->fill_extent(x, y, &width, &height);
    
    return rectangle(x, y, width, height)->ComputeBounds(transform);
}
