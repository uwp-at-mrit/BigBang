#define _USE_MATH_DEFINES
#include <WindowsNumerics.h>

#include "geometry.hpp"

using namespace Microsoft::Graphics::Canvas;
using namespace Microsoft::Graphics::Canvas::Geometry;

using namespace Windows::Foundation;
using namespace Windows::Foundation::Numerics;

static CanvasDevice^ shared_ds = CanvasDevice::GetSharedDevice();

CanvasGeometry^ geometry_rotate(CanvasGeometry^ g, double d) {
    Rect region = g->ComputeBounds();

    return geometry_rotate(g, d,
        region.X + region.Width / 2.0F,
        region.Y + region.Height / 2.0F);
}

CanvasGeometry^ geometry_rotate(CanvasGeometry^ g, double d, float cx, float cy) {
    return g->Transform(make_float3x2_rotation(float(d * M_PI / 180.0), float2(cx, cy)));
}

CanvasGeometry^ geometry_combine(CanvasGeometry^ g1, CanvasGeometry^ g2, CanvasGeometryCombine c) {
    return g1->CombineWith(g2, float3x2::identity(), c);
}

/*************************************************************************************************/
CanvasGeometry^ cylinder_surface(float x, float y, float rx, float ry, float height) {
    auto surface = ref new CanvasPathBuilder(shared_ds);
    float cx = x + rx;
    float cy = y + ry;
    
    surface->BeginFigure(x, y);
    surface->AddArc(float2(cx, cy), rx, ry, float(M_PI), -float(M_PI));
    surface->AddLine(cx + rx, y + height);
    surface->AddArc(float2(cx, cy + height), rx, ry, 0.0F, float(M_PI));
    surface->EndFigure(CanvasFigureLoop::Closed);

    return CanvasGeometry::CreatePath(surface);
}

CanvasGeometry^ pyramid_surface(float x, float y, float rt, float rb, float ry, float height) {
    auto surface = ref new CanvasPathBuilder(shared_ds);
    float cx = x + rt;
    float cy = y + ry;

    surface->BeginFigure(x, y);
    surface->AddArc(float2(cx, cy), rt, ry, float(M_PI), -float(M_PI));
    surface->AddLine(cx + rb, y + height);
    surface->AddArc(float2(cx, cy + height), rb, ry * rb / rt, 0.0F, float(M_PI));
    surface->EndFigure(CanvasFigureLoop::Closed);

    return CanvasGeometry::CreatePath(surface);
}

CanvasGeometry^ rotate_rectangle(float x, float y, float w, float h, double d) {
    return rotate_rectangle(x, y, w, h, d, x + w / 2.0F, y + h / 2.0F);
}

CanvasGeometry^ rotate_rectangle(float x, float y, float w, float h, double d, float cx, float cy) {
    return geometry_rotate(CanvasGeometry::CreateRectangle(shared_ds, Rect(x, y, w, h)), d, cx, cy);
}
