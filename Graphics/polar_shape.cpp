#define _USE_MATH_DEFINES
#include <WindowsNumerics.h>

#include "shape.hpp"
#include "polar_shape.hpp"
#include "transformation.hpp"

using namespace Windows::Foundation;
using namespace Windows::Foundation::Numerics;

using namespace Microsoft::Graphics::Canvas;
using namespace Microsoft::Graphics::Canvas::Text;
using namespace Microsoft::Graphics::Canvas::Geometry;

inline static void circle_point(float radius, double degrees, float* x, float* y) {
	float radians = float(degrees * M_PI / 180.0);

	(*x) = radius * cosf(radians);
	(*y) = radius * sinf(radians);
}

inline static void line_point(float x0, float y0, float x1, float y1, double ratio, float* x, float* y) {
	float flratio = float(ratio);

	(*x) = (x0 - x1) * flratio + x1;
	(*y) = (y0 - y1) * flratio + y1;
}

static CanvasGeometry^ make_masked_triangle(float r, double d, double ratio) {
	auto equilateral_triangle = ref new CanvasPathBuilder(CanvasDevice::GetSharedDevice());
	float x0, y0, x1, y1, x2, y2, x, y;

	circle_point(r, d, &x0, &y0);
	circle_point(r, d + 120.0, &x1, &y1);
	circle_point(r, d - 120.0, &x2, &y2);

	if (ratio > 0.0) { // bottom-up
		equilateral_triangle->BeginFigure(x1, y1);
		equilateral_triangle->AddLine(x2, y2);
		line_point(x0, y0, x2, y2, ratio, &x, &y);
		equilateral_triangle->AddLine(x, y);
		line_point(x0, y0, x1, y1, ratio, &x, &y);
		equilateral_triangle->AddLine(x, y);
		equilateral_triangle->AddLine(x1, y1);
	} else { // top-down
		equilateral_triangle->BeginFigure(x0, y0);
		line_point(x1, y1, x0, y0, -ratio, &x, &y);
		equilateral_triangle->AddLine(x, y);
		line_point(x2, y2, x0, y0, -ratio, &x, &y);
		equilateral_triangle->AddLine(x, y);
		equilateral_triangle->AddLine(x0, y0);
	}

	equilateral_triangle->EndFigure(CanvasFigureLoop::Closed);

	return CanvasGeometry::CreatePath(equilateral_triangle);
}

static CanvasGeometry^ make_masked_sandglass(float r, double d, double ratio) {
	auto glass = ref new CanvasPathBuilder(CanvasDevice::GetSharedDevice());
	float xlt, ylt, xrt, yrt, xlb, ylb, xrb, yrb, x, y;

	circle_point(r, d + 60.0, &xrt, &yrt);
	circle_point(r, d + 120.0, &xlt, &ylt);
	circle_point(r, d - 60.0, &xrb, &yrb);
	circle_point(r, d - 120.0, &xlb, &ylb);

	if (ratio > 0.0) { // bottom-up
		glass->BeginFigure(xrb, yrb);
		glass->AddLine(xlb, ylb);
		line_point(xrt, yrt, xlb, ylb, ratio, &x, &y);
		glass->AddLine(x, y);
		line_point(xlt, ylt, xrb, yrb, ratio, &x, &y);
		glass->AddLine(x, y);
		glass->AddLine(xrb, yrb);
	} else { // top-down
		glass->BeginFigure(xlt, ylt);
		glass->AddLine(xrt, yrt);
		line_point(xlb, ylb, xrt, yrt, -ratio, &x, &y);
		glass->AddLine(x, y);
		line_point(xrb, yrb, xlt, ylt, -ratio, &x, &y);
		glass->AddLine(x, y);
		glass->AddLine(xlt, ylt);
	}

	glass->EndFigure(CanvasFigureLoop::Closed);

	return CanvasGeometry::CreatePath(glass);
}

/*************************************************************************************************/
CanvasGeometry^ polar_axis(float r, double d) {
	auto axis = ref new CanvasPathBuilder(CanvasDevice::GetSharedDevice());
	float x, y;

	axis->BeginFigure(0.0F, 0.0F);
	circle_point(r, d, &x, &y);
	axis->AddLine(x, y);
	axis->EndFigure(CanvasFigureLoop::Open);

	return CanvasGeometry::CreatePath(axis);
}

CanvasGeometry^ polar_pole(float r, double d, float ptr) {
	float x, y;

	circle_point(r, d, &x, &y);
	
	return CanvasGeometry::CreateEllipse(CanvasDevice::GetSharedDevice(), x, y, ptr, ptr);
}

CanvasGeometry^ polar_triangle(float r, double d) {
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

CanvasGeometry^ polar_masked_triangle(float r, double d, double ratio) {
	if (ratio == 0.0) {
		return blank();
	} else if ((ratio <= -1.0) || (ratio >= 1.0)) {
		return polar_triangle(r, d);
	} else {
		return make_masked_triangle(r, d, ratio);
	}
}

CanvasGeometry^ polar_sandglass(float r, double d) {
	auto glass = ref new CanvasPathBuilder(CanvasDevice::GetSharedDevice());
	float x, y;

	circle_point(r, d + 60.0, &x, &y);
	glass->BeginFigure(x, y);
	circle_point(r, d + 120.0, &x, &y);
	glass->AddLine(x, y);
	circle_point(r, d - 60.0, &x, &y);
	glass->AddLine(x, y);
	circle_point(r, d - 120.0, &x, &y);
	glass->AddLine(x, y);
	glass->EndFigure(CanvasFigureLoop::Closed);

	return CanvasGeometry::CreatePath(glass);
}

CanvasGeometry^ polar_masked_sandglass(float r, double d, double ratio) {
	if (ratio == 0.0) {
		return blank();
	} else if ((ratio <= -1.0) || (ratio >= 1.0)) {
		return polar_sandglass(r, d);
	} else {
		return make_masked_sandglass(r, d, ratio);
	}
}

CanvasGeometry^ polar_rectangle(float r, double d) {
	auto frame = ref new CanvasPathBuilder(CanvasDevice::GetSharedDevice());
	float x, y;

	circle_point(r, d + 60.0, &x, &y);
	frame->BeginFigure(x, y);
	circle_point(r, d + 120.0, &x, &y);
	frame->AddLine(x, y);
	circle_point(r, d - 120.0, &x, &y);
	frame->AddLine(x, y);
	circle_point(r, d - 60.0, &x, &y);
	frame->AddLine(x, y);
	frame->EndFigure(CanvasFigureLoop::Closed);

	return CanvasGeometry::CreatePath(frame);
}
