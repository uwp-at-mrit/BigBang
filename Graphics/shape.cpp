#define _USE_MATH_DEFINES
#include <WindowsNumerics.h>

#include "shape.hpp"
#include "transformation.hpp"

using namespace Windows::Foundation;
using namespace Windows::Foundation::Numerics;

using namespace Microsoft::Graphics::Canvas;
using namespace Microsoft::Graphics::Canvas::Text;
using namespace Microsoft::Graphics::Canvas::Geometry;

inline static void circle_point(float radius, double degrees, float* x, float* y) {
	float radians = float(degrees * M_PI / 180.0);

	(*x) = radius * (cosf(radians) + 1.0F);
	(*y) = radius * (sinf(radians) + 1.0F);
}

inline static void line_point(float x0, float y0, float x1, float y1, double ratio, float* x, float* y) {
	float flratio = float(ratio);

	(*x) = (x0 - x1) * flratio + x1;
	(*y) = (y0 - y1) * flratio + y1;
}

/*************************************************************************************************/
CanvasGeometry^ blank() {
    return CanvasGeometry::CreatePath(ref new CanvasPathBuilder(CanvasDevice::GetSharedDevice()));
}

CanvasGeometry^ paragraph(CanvasTextLayout^ tl) {
    float x = tl->LayoutBounds.X;
    float y = tl->LayoutBounds.Y;
    auto layout = CanvasGeometry::CreateText(tl);
    
    if ((x >= 0.0F) && (y >= 0.0F)) {
        return layout;
    } else {
        return geometry_union(blank(), layout, make_translation_matrix(-x, -y));
    }
}

CanvasGeometry^ hline(float x, float y, float l, float th, CanvasStrokeStyle^ style) {
    auto line = ref new CanvasPathBuilder(CanvasDevice::GetSharedDevice());

    line->BeginFigure(x, y);
    line->AddLine(x + l, y);
    line->EndFigure(CanvasFigureLoop::Open);

    return geometry_stroke(CanvasGeometry::CreatePath(line), th, style);
}

CanvasGeometry^ hline(float l, float th, CanvasStrokeStyle^ style) {
    return hline(0.0F, 0.0F, l, th, style);
}

CanvasGeometry^ vline(float x, float y, float l, float th, CanvasStrokeStyle^ style) {
    auto line = ref new CanvasPathBuilder(CanvasDevice::GetSharedDevice());

    line->BeginFigure(x, y);
    line->AddLine(x, y + l);
    line->EndFigure(CanvasFigureLoop::Open);

    return geometry_stroke(CanvasGeometry::CreatePath(line), th, style);
}

CanvasGeometry^ vline(float l, float th, CanvasStrokeStyle^ style) {
    return vline(0.0F, 0.0F, l, th, style);
}

CanvasGeometry^ short_arc(float sx, float sy, float ex, float ey, float rx, float ry, float th) {
    auto arc = ref new CanvasPathBuilder(CanvasDevice::GetSharedDevice());
    
    arc->BeginFigure(sx, sy);
    arc->AddArc(float2(ex, ey), rx, ry, 0.0F, CanvasSweepDirection::Clockwise, CanvasArcSize::Small);
    arc->EndFigure(CanvasFigureLoop::Open);

    return geometry_stroke(CanvasGeometry::CreatePath(arc), th);
}

CanvasGeometry^ long_arc(float sx, float sy, float ex, float ey, float rx, float ry, float th) {
    auto arc = ref new CanvasPathBuilder(CanvasDevice::GetSharedDevice());

    arc->BeginFigure(sx, sy);
    arc->AddArc(float2(ex, ey), rx, ry, 0.0F, CanvasSweepDirection::Clockwise, CanvasArcSize::Large);
    arc->EndFigure(CanvasFigureLoop::Open);

    return geometry_stroke(CanvasGeometry::CreatePath(arc), th);
}

CanvasGeometry^ circle(float cx, float cy, float r) {
    return CanvasGeometry::CreateCircle(CanvasDevice::GetSharedDevice(), cx, cy, r);
}

CanvasGeometry^ ellipse(float cx, float cy, float rx, float ry) {
    return CanvasGeometry::CreateEllipse(CanvasDevice::GetSharedDevice(), cx, cy, rx, ry);
}

CanvasGeometry^ triangle(float r, double d) {
	auto equilateral_triangle = ref new CanvasPathBuilder(CanvasDevice::GetSharedDevice());
	float x, y;

	circle_point(r, d, &x, &y);
	equilateral_triangle->BeginFigure(x, y);
	circle_point(r, d + 120.0, &x, &y);
	equilateral_triangle->AddLine(x, y);
	circle_point(r, d - 120.0, &x, &y);
	equilateral_triangle->AddLine(x, y);
	equilateral_triangle->EndFigure(CanvasFigureLoop::Closed);

	return CanvasGeometry::CreatePath(equilateral_triangle);
}

CanvasGeometry^ trapezoid(float r, double d, double ratio) {
	if (ratio <= 0.0) {
		return blank();
	} else if (ratio >= 1.0) {
		return triangle(r, d);
	} else {
		auto equilateral_trapezoid = ref new CanvasPathBuilder(CanvasDevice::GetSharedDevice());
		float x0, y0, x1, y1, x2, y2, x, y;

		circle_point(r, d, &x0, &y0);
		circle_point(r, d + 120.0, &x1, &y1);
		circle_point(r, d - 120.0, &x2, &y2);

		equilateral_trapezoid->BeginFigure(x1, y1);
		equilateral_trapezoid->AddLine(x2, y2);
		line_point(x0, y0, x2, y2, ratio, &x, &y);
		equilateral_trapezoid->AddLine(x, y);
		line_point(x0, y0, x1, y1, ratio, &x, &y);
		equilateral_trapezoid->AddLine(x, y);
		equilateral_trapezoid->AddLine(x1, y1);

		equilateral_trapezoid->EndFigure(CanvasFigureLoop::Closed);

		return CanvasGeometry::CreatePath(equilateral_trapezoid);
	}
}

CanvasGeometry^ rectangle(float x, float y, float w, float h) {
    return CanvasGeometry::CreateRectangle(CanvasDevice::GetSharedDevice(), Rect(x, y, w, h));
}

CanvasGeometry^ rectangle(float w, float h) {
    return rectangle(0.0F, 0.0F, w, h);
}

CanvasGeometry^ rounded_rectangle(float x, float y, float w, float h, float rx, float ry) {
    float radius_x = (rx < 0.0F) ? -(w * rx) : rx;
    float radius_y = (ry < 0.0F) ? -(h * ry) : ry;

    return CanvasGeometry::CreateRoundedRectangle(CanvasDevice::GetSharedDevice(), Rect(x, y, w, h), radius_x, radius_y);
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

CanvasGeometry^ double_arrow(float x, float y, float arrow_size, float head_size, float spacing, float thickness, CanvasStrokeStyle^ style) {
	auto arrow = ref new CanvasPathBuilder(CanvasDevice::GetSharedDevice());
	float arrowheadsize = head_size * thickness;
	float alignoff = thickness * 1.0F;
	float wingsize = arrowheadsize * 0.5F;
	float out_y = y + wingsize;
	float in_y = out_y + wingsize * ((spacing >= 0.0F) ? spacing : (-spacing * wingsize));
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

CanvasGeometry^ double_arrow(float arrow_size, float arrowhead_size, float spacing, float thickness, CanvasStrokeStyle^ style) {
	return double_arrow(0.0F, 0.0F, arrow_size, arrowhead_size, spacing, thickness, style);
}

Geometry::CanvasGeometry^ stadium(float length, float radius) {
	return stadium(0.0F, 0.0F, length, radius);
}

Microsoft::Graphics::Canvas::Geometry::CanvasGeometry^ stadium(float x, float y, float length, float radius) {
	auto discorectangle = ref new CanvasPathBuilder(CanvasDevice::GetSharedDevice());
	float lx = x + radius;
	float rx = lx + length;
	float by = y + radius + radius;

	discorectangle->BeginFigure(lx, y);
	discorectangle->AddLine(rx, y);
	discorectangle->AddArc(float2(rx, by), radius, radius, 0.0F, CanvasSweepDirection::Clockwise, CanvasArcSize::Small);
	discorectangle->AddLine(lx, by);
	discorectangle->AddArc(float2(lx, y), radius, radius, 0.0F, CanvasSweepDirection::Clockwise, CanvasArcSize::Small);
	discorectangle->EndFigure(CanvasFigureLoop::Closed);

	return CanvasGeometry::CreatePath(discorectangle);
}
