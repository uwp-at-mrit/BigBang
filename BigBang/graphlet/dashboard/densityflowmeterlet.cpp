#include "graphlet/dashboard/densityflowmeterlet.hpp"

#include "math.hpp"
#include "shape.hpp"
#include "paint.hpp"
#include "geometry.hpp"

#include "measure/rhatchmark.hpp"

using namespace WarGrey::SCADA;

using namespace Windows::Foundation;
using namespace Windows::Foundation::Numerics;

using namespace Microsoft::Graphics::Canvas;
using namespace Microsoft::Graphics::Canvas::Brushes;
using namespace Microsoft::Graphics::Canvas::Geometry;

static CanvasSolidColorBrush^ dfmeter_default_pointer_color = Colours::YellowGreen;
static CanvasSolidColorBrush^ dfmeter_default_density_color = Colours::Crimson;
static CanvasSolidColorBrush^ dfmeter_default_flspeed_color = Colours::DodgerBlue;
static CanvasSolidColorBrush^ dfmeter_default_base_color = Colours::DarkSlateGray;

static inline void dfmeter_point(float radius, double v, double vmin, double vmax, double start, double end, float* px, float* py) {
	double percentage = (v - vmin) / (vmax - vmin);
	double degrees = (end - start) * percentage + start;

	circle_point(radius, degrees, px, py);
}

/*************************************************************************************************/
DensityFlowmeterlet::DensityFlowmeterlet(float radius, double degoff, float thickness
	, unsigned int step, unsigned int precision, ICanvasBrush^ pcolor, ICanvasBrush^ dcolor, ICanvasBrush^ fcolor, ICanvasBrush^ bcolor)
	: DensityFlowmeterlet(1.0, 2.0, 0.0, 10.0, radius, degoff, thickness, step, precision, pcolor, dcolor, fcolor, bcolor) {}

DensityFlowmeterlet::DensityFlowmeterlet(double dmin, double dmax, double fmin, double fmax, float radius, double degoff
	, float thickness, unsigned int step, unsigned int precision, ICanvasBrush^ pcolor
	, ICanvasBrush^ dcolor, ICanvasBrush^ fcolor, ICanvasBrush^ bcolor)
	: density_min(dmin), density_max(dmax), flspeed_min(fmin), flspeed_max(fmax)
	, radius(radius), thickness(thickness), step(step), precision(precision)
	, pointer_color(pcolor == nullptr ? dfmeter_default_pointer_color : pcolor)
	, density_color(dcolor == nullptr ? dfmeter_default_density_color : dcolor)
	, flspeed_color(fcolor == nullptr ? dfmeter_default_flspeed_color : fcolor)
	, base_color(bcolor == nullptr ? dfmeter_default_base_color : bcolor) {
	
	this->density_start = 180.0 + degoff;
	this->density_end = 270.0 - degoff;
	this->flspeed_start = -degoff;
	this->flspeed_end = degoff - 90.0;
}

void DensityFlowmeterlet::construct() {
	RHatchMarkMetrics dmetrics, fmetrics;
	float thickoff = this->thickness * 0.5F;
	
	auto density = rhatchmark(this->radius, this->density_start, this->density_end, this->density_min, this->density_max,
		this->step, this->thickness, &dmetrics, this->precision);

	auto flspeed = rhatchmark(this->radius, this->flspeed_start, this->flspeed_end, this->flspeed_min, this->flspeed_max,
		this->step, this->thickness, &fmetrics, this->precision);

	float epr = std::fabsf(dmetrics.arc_sy);
	float ldistance = dmetrics.arc_sx - dmetrics.label_lx;
	float rdistance = fmetrics.label_rx - fmetrics.arc_sx;

	this->pointer_radius = this->radius + dmetrics.hatch_width * 0.618F;
	this->epcy = -std::fminf(dmetrics.label_ty, fmetrics.label_ty) + thickoff;
	this->width = ldistance + rdistance + this->radius + this->thickness;
	this->height = this->epcy + epr + thickoff;

	{ // relocate and construct
		float dx = thickoff - dmetrics.label_lx;
		float fx = (this->width - thickoff) - fmetrics.label_rx;
		float epoff = thickoff;
		float rxdiff = epr - std::fmaxf((dmetrics.arc_sx - dmetrics.label_lx), (fmetrics.label_rx - fmetrics.arc_sx));
		auto endpoint_base = circle(0.0F, 0.0F, epr);
		auto endpoint = circle(0.0F, 0.0F, this->thickness * 1.618F);

		if (rxdiff < 0.0F) {
			epoff = -rxdiff;
		} else {
			dx += rxdiff;
			fx -= rxdiff;
		}

		this->fepcx = epr + epoff;
		this->depcx = this->width - epr - epoff;

		this->density = geometry_freeze(geometry_translate(density, dx, this->epcy));
		this->flspeed = geometry_freeze(geometry_translate(flspeed, fx, this->epcy));

		this->endpoint_bases = geometry_freeze(geometry_union(endpoint_base, this->depcx, this->epcy, endpoint_base, this->fepcx, this->epcy));
		this->endpoints = geometry_freeze(geometry_union(endpoint, this->depcx, this->epcy, endpoint, this->fepcx, this->epcy));
	}

	this->set_values(this->density_max, this->flspeed_max);
}

void DensityFlowmeterlet::fill_extent(float x, float y, float* w, float* h) {
	SET_VALUES(w, this->width, h, this->height);
}

void DensityFlowmeterlet::set_values(double density, double flspeed) {
	float thickoff = this->thickness * 0.5F;
	float r = this->pointer_radius;
	float dx, dy, fx, fy;

	this->_density = density;
	this->_flspeed = flspeed;

	dfmeter_point(r, density, this->density_min, this->density_max, this->density_start, this->density_end, &dx, &dy);
	dfmeter_point(r, flspeed, this->flspeed_min, this->flspeed_max, this->flspeed_start, this->flspeed_end, &fx, &fy);

	this->pointers = geometry_freeze(
		geometry_union(
			line(this->depcx, this->epcy, this->depcx + dx - thickoff, this->epcy + dy + thickoff, this->thickness),
			line(this->fepcx, this->epcy, this->fepcx + fx - thickoff, this->epcy + fy + thickoff, this->thickness)));
}

void DensityFlowmeterlet::draw(CanvasDrawingSession^ ds, float x, float y, float Width, float Height) {
	ds->DrawCachedGeometry(this->endpoint_bases, x, y, this->base_color);
	ds->DrawCachedGeometry(this->endpoints, x, y, this->pointer_color);
	
	ds->DrawCachedGeometry(this->density, x, y, this->density_color);
	ds->DrawCachedGeometry(this->flspeed, x, y, this->flspeed_color);
	
	ds->DrawCachedGeometry(this->pointers, x, y, this->pointer_color);
}
