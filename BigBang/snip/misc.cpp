#include "snip/misc.hpp"
#include "snip.hpp"
#include "shape.hpp"

using namespace WarGrey::SCADA;
using namespace Windows::Foundation;
using namespace Windows::Foundation::Numerics;

using namespace Microsoft::Graphics::Canvas;
using namespace Microsoft::Graphics::Canvas::Geometry;

Rect snip_enclosing_box(Snip* snip, float x, float y, float3x2 transform) {
    float width, height;

    snip->fill_extent(x, y, &width, &height);
    
    return rectangle(x, y, width, height)->ComputeBounds(transform);
}

Rect snip_socket_hflip(Snip* snip, float x, float y, Rect& socket) {
    float width, socket_x;

    snip->fill_extent(x, y, &width, nullptr);

    socket_x = width - (socket.X + socket.Width);
    return Rect{ socket_x, socket.Y, socket.Width, socket.Height };
}

void snip_draw_with_hflipping(Snip* snip, CanvasDrawingSession^ ds, float x, float y, float Width, float Height) {
    float3x2 tf_origin = ds->Transform;
    float2 scale = float2(-1.0F, 1.0F);
    float width;

    snip->fill_extent(x, y, &width, nullptr);
    ds->Transform = tf_origin * make_float3x2_scale(scale);
    snip->draw(ds, (x + width) * scale.x, y, Width, Height);

    ds->Transform = tf_origin;
}
