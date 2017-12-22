#define _USE_MATH_DEFINES
#include <WindowsNumerics.h>

#include "shape.hpp"
#include "geometry.hpp"

using namespace Microsoft::Graphics::Canvas;
using namespace Microsoft::Graphics::Canvas::Text;
using namespace Microsoft::Graphics::Canvas::Geometry;

using namespace Windows::Foundation;
using namespace Windows::Foundation::Numerics;

static CanvasDevice^ shared_ds = CanvasDevice::GetSharedDevice();

/*************************************************************************************************/
CanvasGeometry^ blank() {
    return CanvasGeometry::CreatePath(ref new CanvasPathBuilder(shared_ds));
}

CanvasGeometry^ paragraph(CanvasTextLayout^ tl) {
    float x = tl->LayoutBounds.X;
    float y = tl->LayoutBounds.Y;
    auto layout = CanvasGeometry::CreateText(tl);
    
    if ((x >= 0.0F) && (y >= 0.0F)) {
        return layout;
    } else {
        return geometry_union(blank(), layout, make_float3x2_translation(float2(-x, -y)));
    }
}

CanvasGeometry^ hline(float x, float y, float l, float th, CanvasStrokeStyle^ style) {
    auto line = ref new CanvasPathBuilder(shared_ds);

    line->BeginFigure(x, y);
    line->AddLine(x + l, y);
    line->EndFigure(CanvasFigureLoop::Open);

    return geometry_stroke(CanvasGeometry::CreatePath(line), th, style);
}

CanvasGeometry^ hline(float l, float th, CanvasStrokeStyle^ style) {
    return hline(0.0F, 0.0F, l, th, style);
}

CanvasGeometry^ vline(float x, float y, float l, float th, CanvasStrokeStyle^ style) {
    auto line = ref new CanvasPathBuilder(shared_ds);

    line->BeginFigure(x, y);
    line->AddLine(x, y + l);
    line->EndFigure(CanvasFigureLoop::Open);

    return geometry_stroke(CanvasGeometry::CreatePath(line), th, style);
}

CanvasGeometry^ vline(float l, float th, CanvasStrokeStyle^ style) {
    return vline(0.0F, 0.0F, l, th, style);
}

CanvasGeometry^ short_arc(float sx, float sy, float ex, float ey, float rx, float ry, float th) {
    auto arc = ref new CanvasPathBuilder(shared_ds);
    
    arc->BeginFigure(sx, sy);
    arc->AddArc(float2(ex, ey), rx, ry, 0.0F, CanvasSweepDirection::Clockwise, CanvasArcSize::Small);
    arc->EndFigure(CanvasFigureLoop::Open);

    return geometry_stroke(CanvasGeometry::CreatePath(arc), th);
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

CanvasGeometry^ rectangle(float w, float h) {
    return rectangle(0.0F, 0.0F, w, h);
}

CanvasGeometry^ rounded_rectangle(float x, float y, float w, float h, float rx, float ry) {
    float radius_x = (rx < 0.0F) ? (w * std::abs(rx)) : rx;
    float radius_y = (ry < 0.0F) ? (h * std::abs(ry)) : ry;

    return CanvasGeometry::CreateRoundedRectangle(shared_ds, Rect(x, y, w, h), radius_x, radius_y);
}

CanvasGeometry^ rounded_rectangle(float w, float h, float rx, float ry) {
    return rounded_rectangle(0.0F, 0.0F, w, h, rx, ry);
}

CanvasGeometry^ rotate_rectangle(float x, float y, float w, float h, double d) {
    return rotate_rectangle(x, y, w, h, d, x + w * 0.5F, y + h * 0.5F);
}

CanvasGeometry^ rotate_rectangle(float w, float h, double d) {
    return rotate_rectangle(0.0F, 0.0F, w, h, d, w * 0.5F, h * 0.5F);
}

CanvasGeometry^ rotate_rectangle(float x, float y, float w, float h, double d, float cx, float cy) {
    return geometry_rotate(rectangle(x, y, w, h), d, cx, cy);
}

CanvasGeometry^ rotate_rectangle(float w, float h, double d, float cx, float cy) {
    return rotate_rectangle(0.0F, 0.0F, w, h, d, cx, cy);
}

CanvasGeometry^ double_arrow(float x, float y, float arrow_size, float head_size, float thickness, CanvasStrokeStyle^ style) {
	auto arrow = ref new CanvasPathBuilder(shared_ds);
	float arrowheadsize = head_size * thickness;
	float alignoff = thickness * 1.0F;
	float wingsize = arrowheadsize * 0.5F;
	float out_y = y + wingsize;
	float in_y = out_y + wingsize + 1.0F;
	float end_x = x + arrow_size;

	arrow->BeginFigure(x + arrowheadsize, y);
	arrow->AddLine(x + alignoff, out_y);
	arrow->AddLine(end_x, out_y);
	arrow->EndFigure(CanvasFigureLoop::Open);

	arrow->BeginFigure(x, in_y);
	arrow->AddLine(end_x - alignoff, in_y);
	arrow->AddLine(end_x - arrowheadsize, in_y + wingsize);
	arrow->EndFigure(CanvasFigureLoop::Open);

	return geometry_stroke(CanvasGeometry::CreatePath(arrow), thickness);
}

CanvasGeometry^ double_arrow(float length, float arrowhead_size, float thickness, CanvasStrokeStyle^ style) {
	return double_arrow(0.0F, 0.0F, length, arrowhead_size, thickness, style);
}

/*************************************************************************************************/
CanvasGeometry^ cylinder_tb_surface(float x, float y, float rx, float ry, float height) {
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

CanvasGeometry^ cylinder_rl_surface(float x, float y, float rx, float ry, float width) {
    auto surface = ref new CanvasPathBuilder(shared_ds);
    float cx = x + rx;
    float cy = y + ry;

    surface->BeginFigure(cx, y);
    surface->AddArc(float2(cx, cy), rx, ry, -float(M_PI_2), -float(M_PI));
    surface->AddLine(cx + width, cy + ry);
    surface->AddArc(float2(cx + width, cy), rx, ry, float(M_PI_2), float(M_PI));
    surface->EndFigure(CanvasFigureLoop::Closed);

    return CanvasGeometry::CreatePath(surface);
}

CanvasGeometry^ cylinder_lr_surface(float x, float y, float rx, float ry, float width) {
    auto surface = ref new CanvasPathBuilder(shared_ds);
    float cx = x + rx;
    float cy = y + ry;

    surface->BeginFigure(cx, y);
    surface->AddArc(float2(cx, cy), rx, ry, -float(M_PI_2), float(M_PI));
    surface->AddLine(cx + width, cy + ry);
    surface->AddArc(float2(cx + width, cy), rx, ry, float(M_PI_2), -float(M_PI));
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

CanvasGeometry^ cylinder_tb_surface(float rx, float ry, float height) {
    return cylinder_tb_surface(0.0F, 0.0F, rx, ry, height);
}

CanvasGeometry^ cylinder_rl_surface(float rx, float ry, float width) {
    return cylinder_rl_surface(0.0F, 0.0F, rx, ry, width);
}

CanvasGeometry^ cylinder_lr_surface(float rx, float ry, float width) {
    return cylinder_lr_surface(0.0F, 0.0F, rx, ry, width);
}

CanvasGeometry^ pyramid_surface(float rt, float rb, float ry, float height) {
    return pyramid_surface(0.0F, 0.0F, rt, rb, ry, height);
}
