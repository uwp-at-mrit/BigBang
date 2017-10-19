#define _USE_MATH_DEFINES
#include <WindowsNumerics.h>

#include "geometry.hpp"
#include "rsyslog.hpp"

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

CanvasGeometry^ geometry_stroke(CanvasGeometry^ g, float thickness) {
    return g->Stroke(thickness);
}

CanvasGeometry^ geometry_substract(CanvasGeometry^ g1, CanvasGeometry^ g2) {
    return g1->CombineWith(g2, float3x2::identity(), CanvasGeometryCombine::Exclude);
}

CanvasGeometry^ geometry_intersect(CanvasGeometry^ g1, CanvasGeometry^ g2) {
    return g1->CombineWith(g2, float3x2::identity(), CanvasGeometryCombine::Intersect);
}

CanvasGeometry^ geometry_union(CanvasGeometry^ g1, CanvasGeometry^ g2) {
    return g1->CombineWith(g2, float3x2::identity(), CanvasGeometryCombine::Union);
}

CanvasGeometry^ geometry_xor(CanvasGeometry^ g1, CanvasGeometry^ g2) {
    return g1->CombineWith(g2, float3x2::identity(), CanvasGeometryCombine::Xor);
}

CanvasGeometry^ geometry_substract(CanvasGeometry^ g1, CanvasGeometry^ g2, float3x2 t) {
    return g1->CombineWith(g2, t, CanvasGeometryCombine::Exclude);
}

CanvasGeometry^ geometry_intersect(CanvasGeometry^ g1, CanvasGeometry^ g2, float3x2 t) {
    return g1->CombineWith(g2, t, CanvasGeometryCombine::Intersect);
}

CanvasGeometry^ geometry_union(CanvasGeometry^ g1, CanvasGeometry^ g2, float3x2 t) {
    return g1->CombineWith(g2, t, CanvasGeometryCombine::Union);
}

CanvasGeometry^ geometry_xor(CanvasGeometry^ g1, CanvasGeometry^ g2, float3x2 t) {
    return g1->CombineWith(g2, t, CanvasGeometryCombine::Xor);
}

/*************************************************************************************************/
CanvasCachedGeometry^ geometry_freeze(CanvasGeometry^ geometry) {
    return CanvasCachedGeometry::CreateFill(geometry);
}

/*************************************************************************************************/
CanvasGeometry^ blank() {
    return CanvasGeometry::CreatePath(ref new CanvasPathBuilder(shared_ds));
}

CanvasGeometry^ hline(float x, float y, float l, float th) {
    auto line = ref new CanvasPathBuilder(shared_ds);

    line->BeginFigure(x, y);
    line->AddLine(x + l, y);
    line->EndFigure(CanvasFigureLoop::Open);

    return geometry_stroke(CanvasGeometry::CreatePath(line), th);
}

CanvasGeometry^ vline(float x, float y, float l, float th) {
    auto line = ref new CanvasPathBuilder(shared_ds);

    line->BeginFigure(x, y);
    line->AddLine(x, y + l);
    line->EndFigure(CanvasFigureLoop::Open);

    return geometry_stroke(CanvasGeometry::CreatePath(line), th);
}

CanvasGeometry^ long_arc(float sx, float sy, float ex, float ey, float rx, float ry, float th) {
    auto arc = ref new CanvasPathBuilder(shared_ds);
    
    arc->BeginFigure(sx, sy);
    arc->AddArc(float2(ex, ey), rx, ry, 0.0F, CanvasSweepDirection::Clockwise, CanvasArcSize::Large);
    arc->EndFigure(CanvasFigureLoop::Open);

    return geometry_stroke(CanvasGeometry::CreatePath(arc), th);
}

CanvasGeometry^ circle(float cx, float cy, float r) {
    return CanvasGeometry::CreateCircle(shared_ds, cx, cy, r);
}

CanvasGeometry^ ellipse(float cx, float cy, float rx, float ry) {
    return CanvasGeometry::CreateEllipse(shared_ds, cx, cy, rx, ry);
}

CanvasGeometry^ rectangle(float x, float y, float w, float h) {
    return CanvasGeometry::CreateRectangle(shared_ds, Rect(x, y, w, h));
}

CanvasGeometry^ rounded_rectangle(float x, float y, float w, float h, float rx, float ry) {
    float radius_x = (rx < 0.0F) ? (w * std::abs(rx)) : rx;
    float radius_y = (ry < 0.0F) ? (h * std::abs(ry)) : ry;

    return CanvasGeometry::CreateRoundedRectangle(shared_ds, Rect(x, y, w, h), radius_x, radius_y);
}

CanvasGeometry^ rotate_rectangle(float x, float y, float w, float h, double d) {
    return rotate_rectangle(x, y, w, h, d, x + w / 2.0F, y + h / 2.0F);
}

CanvasGeometry^ rotate_rectangle(float x, float y, float w, float h, double d, float cx, float cy) {
    return geometry_rotate(rectangle(x, y, w, h), d, cx, cy);
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
