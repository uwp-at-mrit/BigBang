#include "shape.hpp"
#include "math.hpp"

using namespace WarGrey::SCADA;

using namespace Windows::Foundation;
using namespace Windows::Foundation::Numerics;

using namespace Microsoft::Graphics::Canvas;
using namespace Microsoft::Graphics::Canvas::Text;
using namespace Microsoft::Graphics::Canvas::Geometry;

static inline Rect smart_rect(float x, float y, float width, float height) {
	if (width < 0.0F) {
		x = x + width;
		width = -width;
	}

	if (height < 0.0F) {
		y = y + height;
		height = -height;
	}

	return Rect(x, y, width, height);
}

/*************************************************************************************************/
CanvasGeometry^ WarGrey::SCADA::blank() {
    return CanvasGeometry::CreatePath(ref new CanvasPathBuilder(CanvasDevice::GetSharedDevice()));
}

CanvasGeometry^ WarGrey::SCADA::paragraph(CanvasTextLayout^ tl, bool adjust) {
    CanvasGeometry^ layout = CanvasGeometry::CreateText(tl);
	float x = tl->LayoutBounds.X;
	float y = tl->LayoutBounds.Y;
    
    if (adjust && ((x < 0.0F) || (y < 0.0F))) {
		float xoff = (x >= 0.0F) ? x : -x;
		float yoff = (y >= 0.0F) ? y : -y;

        layout = geometry_union(blank(), layout, xoff, yoff);
    }

	return layout;
}

CanvasGeometry^ WarGrey::SCADA::line(float sx, float sy, float ex, float ey, float th, CanvasStrokeStyle^ style) {
	auto line = ref new CanvasPathBuilder(CanvasDevice::GetSharedDevice());

	line->BeginFigure(sx, sy);
	line->AddLine(ex, ey);
	line->EndFigure(CanvasFigureLoop::Open);

	return geometry_stroke(CanvasGeometry::CreatePath(line), th, style);
}

CanvasGeometry^ WarGrey::SCADA::line(float ex, float ey, float th, CanvasStrokeStyle^ style) {
	return line(0.0F, 0.0F, ex, ey, th, style);
}

CanvasGeometry^ WarGrey::SCADA::hline(float x, float y, float l, float th, CanvasStrokeStyle^ style) {
	return line(x, y, x + l, y, th, style);
}

CanvasGeometry^ WarGrey::SCADA::hline(float l, float th, CanvasStrokeStyle^ style) {
    return hline(0.0F, 0.0F, l, th, style);
}

CanvasGeometry^ WarGrey::SCADA::vline(float x, float y, float l, float th, CanvasStrokeStyle^ style) {
	return line(x, y, x, y + l, th, style);
}

CanvasGeometry^ WarGrey::SCADA::vline(float l, float th, CanvasStrokeStyle^ style) {
    return vline(0.0F, 0.0F, l, th, style);
}

CanvasGeometry^ WarGrey::SCADA::arc(double start, double end, float radiusX, float radiusY, float th, CanvasStrokeStyle^ style) {
	return arc(0.0F, 0.0F, start, end, radiusX, radiusY, th, style);
}

CanvasGeometry^ WarGrey::SCADA::arc(float cx, float cy, double start, double end, float radiusX, float radiusY, float th, CanvasStrokeStyle^ style) {
	auto arc_path = ref new CanvasPathBuilder(CanvasDevice::GetSharedDevice());
	float rstart = degrees_to_radians(start);
	float rsweep = degrees_to_radians(end - start);
	float startx, starty;

	ellipse_point(radiusX, radiusY, start, &startx, &starty);

	arc_path->BeginFigure(cx + startx, cy + starty);
	arc_path->AddArc(float2(cx, cy), radiusX, radiusY, rstart, rsweep);
	arc_path->EndFigure(CanvasFigureLoop::Open);

	return geometry_stroke(CanvasGeometry::CreatePath(arc_path), th, style);
}

CanvasGeometry^ WarGrey::SCADA::short_arc(double start, double end, float radiusX, float radiusY, float th, CanvasStrokeStyle^ style) {
	float sx, sy, ex, ey;

	ellipse_point(radiusX, radiusY, start, &sx, &sy);
	ellipse_point(radiusX, radiusY, end, &ex, &ey);

	return short_arc(sx, sy, ex, ey, radiusX, radiusY, th, style);
}

CanvasGeometry^ WarGrey::SCADA::short_arc(float sx, float sy, float ex, float ey, float rx, float ry, float th, CanvasStrokeStyle^ style) {
    auto arc = ref new CanvasPathBuilder(CanvasDevice::GetSharedDevice());
    
    arc->BeginFigure(sx, sy);
    arc->AddArc(float2(ex, ey), rx, ry, 0.0F, CanvasSweepDirection::Clockwise, CanvasArcSize::Small);
    arc->EndFigure(CanvasFigureLoop::Open);

    return geometry_stroke(CanvasGeometry::CreatePath(arc), th, style);
}

CanvasGeometry^ WarGrey::SCADA::long_arc(double start, double end, float radiusX, float radiusY, float th, CanvasStrokeStyle^ style) {
	float sx, sy, ex, ey;

	ellipse_point(radiusX, radiusY, start, &sx, &sy);
	ellipse_point(radiusX, radiusY, end, &ex, &ey);

	return long_arc(sx, sy, ex, ey, radiusX, radiusY, th, style);
}

CanvasGeometry^ WarGrey::SCADA::long_arc(float sx, float sy, float ex, float ey, float rx, float ry, float th, CanvasStrokeStyle^ style) {
    auto arc = ref new CanvasPathBuilder(CanvasDevice::GetSharedDevice());

    arc->BeginFigure(sx, sy);
    arc->AddArc(float2(ex, ey), rx, ry, 0.0F, CanvasSweepDirection::Clockwise, CanvasArcSize::Large);
    arc->EndFigure(CanvasFigureLoop::Open);

    return geometry_stroke(CanvasGeometry::CreatePath(arc), th, style);
}

CanvasGeometry^ WarGrey::SCADA::circle(float r) {
	return circle(r * 0.5F, r * 0.5F, r);
}

CanvasGeometry^ WarGrey::SCADA::circle(float cx, float cy, float r) {
    return CanvasGeometry::CreateCircle(CanvasDevice::GetSharedDevice(), cx, cy, r);
}

CanvasGeometry^ WarGrey::SCADA::ellipse(float rx, float ry) {
	return ellipse(rx * 0.5F, ry * 0.5F, rx, ry);
}

CanvasGeometry^ WarGrey::SCADA::ellipse(float cx, float cy, float rx, float ry) {
    return CanvasGeometry::CreateEllipse(CanvasDevice::GetSharedDevice(), cx, cy, rx, ry);
}

CanvasGeometry^ WarGrey::SCADA::sector(double start, double end, float radiusX, float radiusY) {
	return sector(0.0F, 0.0F, start, end, radiusX, radiusY);
}

CanvasGeometry^ WarGrey::SCADA::sector(float cx, float cy, double start, double end, float radiusX, float maybe_radiusY) {
	return masked_sector(cx, cy, start, end, 0.0, radiusX, maybe_radiusY);
}

CanvasGeometry^ WarGrey::SCADA::masked_sector(double start, double end, double ratio, float radiusX, float radiusY) {
	return masked_sector(0.0F, 0.0F, start, end, ratio, radiusX, radiusY);
}

CanvasGeometry^ WarGrey::SCADA::masked_sector(float cx, float cy, double start, double end, double ratio, float radiusX, float maybe_radiusY) {
	auto sector_path = ref new CanvasPathBuilder(CanvasDevice::GetSharedDevice());
	float radiusY = (maybe_radiusY <= 0.0F) ? radiusX : maybe_radiusY;
	float mradiusX = radiusX * float(ratio);
	float mradiusY = radiusY * float(ratio);
	float rstart = degrees_to_radians(start);
	float rsweep = degrees_to_radians(end - start);
	float startx, starty, mendx, mendy;

	ellipse_point(radiusX, radiusY, start, &startx, &starty);
	ellipse_point(mradiusX, mradiusY, end, &mendx, &mendy);

	sector_path->BeginFigure(cx + startx, cy + starty);
	sector_path->AddArc(float2(cx, cy), radiusX, radiusY, rstart, rsweep);
	sector_path->AddLine(cx + mendx, cy + mendy);
	sector_path->AddArc(float2(cx, cy), mradiusX, mradiusY, rstart + rsweep, -rsweep);
	sector_path->AddLine(cx + startx, cy + starty);
	sector_path->EndFigure(CanvasFigureLoop::Closed);

	return CanvasGeometry::CreatePath(sector_path);
}

CanvasGeometry^ WarGrey::SCADA::segment(double start, double end, float radiusX, float radiusY) {
	return segment(0.0F, 0.0F, start, end, radiusX, radiusY);
}

CanvasGeometry^ WarGrey::SCADA::segment(float cx, float cy, double start, double end, float radiusX, float maybe_radiusY) {
	auto sector_path = ref new CanvasPathBuilder(CanvasDevice::GetSharedDevice());
	float radiusY = (maybe_radiusY <= 0.0F) ? radiusX : maybe_radiusY;
	float rstart = degrees_to_radians(start);
	float rsweep = degrees_to_radians(end - start);
	float startx, starty;

	ellipse_point(radiusX, radiusY, start, &startx, &starty);

	sector_path->BeginFigure(cx + startx, cy + starty);
	sector_path->AddArc(float2(cx, cy), radiusX, radiusY, rstart, rsweep);
	sector_path->AddLine(cx + startx, cy + starty);
	sector_path->EndFigure(CanvasFigureLoop::Closed);

	return CanvasGeometry::CreatePath(sector_path);
}

CanvasGeometry^ WarGrey::SCADA::triangle(float x1, float y1, float x2, float y2) {
	auto ra_triangle = ref new CanvasPathBuilder(CanvasDevice::GetSharedDevice());

	ra_triangle->BeginFigure(x1, y1);
	ra_triangle->AddLine(x1, y2);
	ra_triangle->AddLine(x2, y2);
	ra_triangle->AddLine(x1, y1);
	ra_triangle->EndFigure(CanvasFigureLoop::Closed);

	return CanvasGeometry::CreatePath(ra_triangle);
}

CanvasGeometry^ WarGrey::SCADA::rectangle(Rect& region) {
	return CanvasGeometry::CreateRectangle(CanvasDevice::GetSharedDevice(), region);
}

CanvasGeometry^ WarGrey::SCADA::rectangle(float x, float y, float w, float h) {
	return rectangle(smart_rect(x, y, w, h));
}

CanvasGeometry^ WarGrey::SCADA::rectangle(float w, float h) {
    return rectangle(smart_rect(0.0F, 0.0F, w, h));
}

CanvasGeometry^ WarGrey::SCADA::rounded_rectangle(float x, float y, float w, float h, float rx, float ry) {
    float radius_x = (rx < 0.0F) ? -(w * rx) : rx;
    float radius_y = (ry < 0.0F) ? -(h * ry) : ry;

    return CanvasGeometry::CreateRoundedRectangle(CanvasDevice::GetSharedDevice(), smart_rect(x, y, w, h), radius_x, radius_y);
}

CanvasGeometry^ WarGrey::SCADA::rotate_rectangle(float x, float y, float w, float h, double d) {
    return rotate_rectangle(x, y, w, h, d, x + w * 0.5F, y + h * 0.5F);
}

CanvasGeometry^ WarGrey::SCADA::rotate_rectangle(float w, float h, double d) {
    return rotate_rectangle(0.0F, 0.0F, w, h, d, w * 0.5F, h * 0.5F);
}

CanvasGeometry^ WarGrey::SCADA::rotate_rectangle(float x, float y, float w, float h, double d, float cx, float cy) {
    return geometry_rotate(rectangle(x, y, w, h), d, cx, cy);
}

CanvasGeometry^ WarGrey::SCADA::rotate_rectangle(float w, float h, double d, float cx, float cy) {
    return rotate_rectangle(0.0F, 0.0F, w, h, d, cx, cy);
}

CanvasGeometry^ WarGrey::SCADA::double_arrow(float x, float y, float arrow_size, float head_size, float spacing, float thickness, CanvasStrokeStyle^ style) {
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

CanvasGeometry^ WarGrey::SCADA::double_arrow(float arrow_size, float arrowhead_size, float spacing, float thickness, CanvasStrokeStyle^ style) {
	return double_arrow(0.0F, 0.0F, arrow_size, arrowhead_size, spacing, thickness, style);
}

CanvasGeometry^ WarGrey::SCADA::stadium(float length, float radius) {
	return stadium(0.0F, 0.0F, length, radius);
}

CanvasGeometry^ WarGrey::SCADA::stadium(float x, float y, float length, float radius) {
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
