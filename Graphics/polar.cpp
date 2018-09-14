#include "math.hpp"
#include "shape.hpp"
#include "polar.hpp"
#include "transformation.hpp"

using namespace WarGrey::SCADA;

using namespace Windows::Foundation;
using namespace Windows::Foundation::Numerics;

using namespace Microsoft::Graphics::Canvas;
using namespace Microsoft::Graphics::Canvas::Text;
using namespace Microsoft::Graphics::Canvas::Geometry;

static CanvasGeometry^ make_masked_triangle(float rx, float ry, double d, double ratio) {
	auto equilateral_triangle = ref new CanvasPathBuilder(CanvasDevice::GetSharedDevice());
	float x0, y0, x1, y1, x2, y2, x, y;

	ellipse_point(rx, ry, d, &x0, &y0);
	ellipse_point(rx, ry, d + 120.0, &x1, &y1);
	ellipse_point(rx, ry, d - 120.0, &x2, &y2);

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

static CanvasGeometry^ make_masked_sandglass(float rx, float ry, double d, double ratio) {
	auto glass = ref new CanvasPathBuilder(CanvasDevice::GetSharedDevice());
	float xlt, ylt, xrt, yrt, xlb, ylb, xrb, yrb, x, y;

	ellipse_point(rx, ry, d + 60.0, &xrt, &yrt);
	ellipse_point(rx, ry, d + 120.0, &xlt, &ylt);
	ellipse_point(rx, ry, d - 60.0, &xrb, &yrb);
	ellipse_point(rx, ry, d - 120.0, &xlb, &ylb);

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

CanvasGeometry^ make_masked_rectangle(float rx, float ry, double a, double d, double ratio) {
	auto frame = ref new CanvasPathBuilder(CanvasDevice::GetSharedDevice());
	double theta = 180.0 - a;
	float xrt, yrt, xlt, ylt, xlb, ylb, xrb, yrb, x, y;

	ellipse_point(rx, ry, d + a, &xrt, &yrt);
	ellipse_point(rx, ry, d + theta, &xlt, &ylt);
	ellipse_point(rx, ry, d - theta, &xlb, &ylb);
	ellipse_point(rx, ry, d - a, &xrb, &yrb);
	
	if (ratio > 0.0) { // bottom-up
		frame->BeginFigure(xrb, yrb);
		frame->AddLine(xlb, ylb);
		line_point(xlt, ylt, xlb, ylb, ratio, &x, &y);
		frame->AddLine(x, y);
		line_point(xrt, yrt, xrb, yrb, ratio, &x, &y);
		frame->AddLine(x, y);
		frame->AddLine(xrb, yrb);
	} else { // top-down
		frame->BeginFigure(xlt, ylt);
		frame->AddLine(xrt, yrt);
		line_point(xrb, yrb, xrt, yrt, -ratio, &x, &y);
		frame->AddLine(x, y);
		line_point(xlb, ylb, xlt, ylt, -ratio, &x, &y);
		frame->AddLine(x, y);
		frame->AddLine(xlt, ylt);
	}

	frame->EndFigure(CanvasFigureLoop::Closed);

	return CanvasGeometry::CreatePath(frame);
}

/*************************************************************************************************/
CanvasGeometry^ WarGrey::SCADA::polar_axis(float r, double d) {
	return polar_axis(r, r, d);
}

CanvasGeometry^ WarGrey::SCADA::polar_axis(float rx, float ry, double d) {
	auto axis = ref new CanvasPathBuilder(CanvasDevice::GetSharedDevice());
	float x, y;

	axis->BeginFigure(0.0F, 0.0F);
	ellipse_point(rx, ry, d, &x, &y);
	axis->AddLine(x, y);
	axis->EndFigure(CanvasFigureLoop::Open);

	return CanvasGeometry::CreatePath(axis);
}

CanvasGeometry^ WarGrey::SCADA::polar_pole(float r, double d, float ptr) {
	return polar_pole(r, r, d, ptr);
}

CanvasGeometry^ WarGrey::SCADA::polar_pole(float rx, float ry, double d, float ptr) {
	float x, y;

	ellipse_point(rx, ry, d, &x, &y);
	
	return CanvasGeometry::CreateEllipse(CanvasDevice::GetSharedDevice(), x, y, ptr, ptr);
}

CanvasGeometry^ WarGrey::SCADA::polar_line(float r, double sd, double ed) {
	return polar_line(r, r, sd, ed);
}

CanvasGeometry^ WarGrey::SCADA::polar_line(float rx, float ry, double sd, double ed) {
	auto axis = ref new CanvasPathBuilder(CanvasDevice::GetSharedDevice());
	float x1, y1, x2, y2;

	ellipse_point(rx, ry, sd, &x1, &y1);
	ellipse_point(rx, ry, ed, &x2, &y2);

	axis->BeginFigure(x1, y1);
	axis->AddLine(x2, y2);
	axis->EndFigure(CanvasFigureLoop::Open);

	return CanvasGeometry::CreatePath(axis);
}

CanvasGeometry^ WarGrey::SCADA::polar_arrowhead(float r, double d) {
	return polar_arrowhead(r, r, d);
}

CanvasGeometry^ WarGrey::SCADA::polar_arrowhead(float rx, float ry, double d) {
	auto arrowhead = ref new CanvasPathBuilder(CanvasDevice::GetSharedDevice());
	float x, y;

	ellipse_point(rx, ry, d, &x, &y);
	arrowhead->BeginFigure(x, y);
	ellipse_point(rx, ry, d + 120.0, &x, &y);
	arrowhead->AddLine(x, y);
	arrowhead->AddLine(0.0F, 0.0F);
	ellipse_point(rx, ry, d - 120.0, &x, &y);
	arrowhead->AddLine(x, y);
	arrowhead->EndFigure(CanvasFigureLoop::Closed);

	return CanvasGeometry::CreatePath(arrowhead);
}

CanvasGeometry^ WarGrey::SCADA::polar_triangle(float r, double d) {
	return polar_triangle(r, r, d);
}

CanvasGeometry^ WarGrey::SCADA::polar_triangle(float rx, float ry, double d) {
	auto equilateral_triangle = ref new CanvasPathBuilder(CanvasDevice::GetSharedDevice());
	float x, y;

	ellipse_point(rx, ry, d, &x, &y);
	equilateral_triangle->BeginFigure(x, y);
	ellipse_point(rx, ry, d + 120.0, &x, &y);
	equilateral_triangle->AddLine(x, y);
	ellipse_point(rx, ry, d - 120.0, &x, &y);
	equilateral_triangle->AddLine(x, y);
	equilateral_triangle->EndFigure(CanvasFigureLoop::Closed);

	return CanvasGeometry::CreatePath(equilateral_triangle);
}

CanvasGeometry^ WarGrey::SCADA::polar_masked_triangle(float r, double d, double ratio) {
	return polar_masked_triangle(r, r, d, ratio);
}

CanvasGeometry^ WarGrey::SCADA::polar_masked_triangle(float rx, float ry, double d, double ratio) {
	if (ratio == 0.0) {
		return blank();
	} else if ((ratio <= -1.0) || (ratio >= 1.0)) {
		return polar_triangle(rx, ry, d);
	} else {
		return make_masked_triangle(rx, ry, d, ratio);
	}
}

CanvasGeometry^ WarGrey::SCADA::polar_sandglass(float r, double d) {
	return polar_sandglass(r, r, d);
}

CanvasGeometry^ WarGrey::SCADA::polar_sandglass(float rx, float ry, double d) {
	auto glass = ref new CanvasPathBuilder(CanvasDevice::GetSharedDevice());
	float x, y;

	ellipse_point(rx, ry, d + 60.0, &x, &y);
	glass->BeginFigure(x, y);
	ellipse_point(rx, ry, d + 120.0, &x, &y);
	glass->AddLine(x, y);
	ellipse_point(rx, ry, d - 60.0, &x, &y);
	glass->AddLine(x, y);
	ellipse_point(rx, ry, d - 120.0, &x, &y);
	glass->AddLine(x, y);
	glass->EndFigure(CanvasFigureLoop::Closed);

	return CanvasGeometry::CreatePath(glass);
}

CanvasGeometry^ WarGrey::SCADA::polar_masked_sandglass(float r, double d, double ratio) {
	return polar_masked_sandglass(r, r, d, ratio);
}

CanvasGeometry^ WarGrey::SCADA::polar_masked_sandglass(float rx, float ry, double d, double ratio) {
	if (ratio == 0.0) {
		return blank();
	} else if ((ratio <= -1.0) || (ratio >= 1.0)) {
		return polar_sandglass(rx, ry, d);
	} else {
		return make_masked_sandglass(rx, ry, d, ratio);
	}
}

CanvasGeometry^ WarGrey::SCADA::polar_rectangle(float r, double alpha, double rotation) {
	return polar_rectangle(r, r, alpha, rotation);
}

CanvasGeometry^ WarGrey::SCADA::polar_rectangle(float rx, float ry, double alpha, double rotation) {
	auto frame = ref new CanvasPathBuilder(CanvasDevice::GetSharedDevice());
	double theta = 180.0 - alpha;
	float x, y;

	ellipse_point(rx, ry, rotation + alpha, &x, &y);
	frame->BeginFigure(x, y);
	ellipse_point(rx, ry, rotation + theta, &x, &y);
	frame->AddLine(x, y);
	ellipse_point(rx, ry, rotation - theta, &x, &y);
	frame->AddLine(x, y);
	ellipse_point(rx, ry, rotation - alpha, &x, &y);
	frame->AddLine(x, y);
	frame->EndFigure(CanvasFigureLoop::Closed);

	return CanvasGeometry::CreatePath(frame);
}

CanvasGeometry^ WarGrey::SCADA::polar_masked_rectangle(float r, double alpha, double rotation, double ratio) {
	return polar_masked_rectangle(r, r, alpha, rotation, ratio);
}

CanvasGeometry^ WarGrey::SCADA::polar_masked_rectangle(float rx, float ry, double alpha, double rotation, double ratio) {
	if (ratio == 0.0) {
		return blank();
	} else if ((ratio <= -1.0) || (ratio >= 1.0)) {
		return polar_rectangle(rx, ry, alpha, rotation);
	} else {
		return make_masked_rectangle(rx, ry, alpha, rotation, ratio);
	}
}
