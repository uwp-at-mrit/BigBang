#include "graphlet/dashboard/densityflowmeterlet.hpp"

#include "math.hpp"
#include "shape.hpp"
#include "paint.hpp"
#include "geometry.hpp"

#include "tongue.hpp"
#include "string.hpp"

#include "measure/rhatchmark.hpp"

using namespace WarGrey::SCADA;

using namespace Windows::Foundation;
using namespace Windows::Foundation::Numerics;

using namespace Microsoft::Graphics::Canvas;
using namespace Microsoft::Graphics::Canvas::Brushes;
using namespace Microsoft::Graphics::Canvas::Geometry;

static CanvasSolidColorBrush^ dfmeter_default_pointer_color = Colours::GreenYellow;
static CanvasSolidColorBrush^ dfmeter_default_density_color = Colours::Crimson;
static CanvasSolidColorBrush^ dfmeter_default_flspeed_color = Colours::DodgerBlue;
static CanvasSolidColorBrush^ dfmeter_default_base_color = Colours::DarkSlateGray;

static inline void dfmeter_point(float radius, double v, double vmin, double vmax, double start, double end, float* px, float* py) {
	double percentage = std::fmax(std::fmin((v - vmin) / (vmax - vmin), 1.0), 0.0);
	double degrees = (end - start) * percentage + start;

	circle_point(radius, degrees, px, py);
}

/*************************************************************************************************/
DensitySpeedmeterlet::DensitySpeedmeterlet(float radius, double degoff, float thickness
	, unsigned int step, unsigned int precision, ICanvasBrush^ pcolor, ICanvasBrush^ dcolor, ICanvasBrush^ fcolor, ICanvasBrush^ bcolor)
	: DensitySpeedmeterlet(1.0, 2.0, 0.0, 10.0, radius, degoff, thickness, step, precision, pcolor, dcolor, fcolor, bcolor) {}

DensitySpeedmeterlet::DensitySpeedmeterlet(double dmin, double dmax, double fmin, double fmax, float radius, double degoff
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
	this->flspeed_start = 0.0 - degoff;
	this->flspeed_end = degoff - 90.0;
}

void DensitySpeedmeterlet::construct() {
	RHatchMarkMetrics dmetrics, fmetrics;
	float thickoff = this->thickness * 0.5F;
	
	auto density = rhatchmark(this->radius, this->density_start, this->density_end, this->density_min, this->density_max,
		this->step, this->thickness, &dmetrics, this->precision);

	auto flspeed = rhatchmark(this->radius, this->flspeed_start, this->flspeed_end, this->flspeed_min, this->flspeed_max,
		this->step, this->thickness, &fmetrics, this->precision);

	float epr = std::fabsf(dmetrics.arc_sy);
	float ldistance = dmetrics.arc_sx - dmetrics.label_lx;
	float rdistance = fmetrics.label_rx - fmetrics.arc_sx;

	this->yoff = -std::fminf(dmetrics.label_ty, fmetrics.label_ty) + thickoff;
	this->epcxoff = epr + thickoff;
	this->width = ldistance + rdistance + this->radius + this->thickness;
	this->height = this->yoff + this->epcxoff;
	this->dxoff = thickoff - dmetrics.label_lx;
	this->fxoff = (this->width - thickoff) - fmetrics.label_rx;

	{ // relocate and construct
		float epoff = thickoff;
		float rxdiff = epr - std::fmaxf((dmetrics.arc_sx - dmetrics.label_lx), (fmetrics.label_rx - fmetrics.arc_sx));
		auto endpoint_base = circle(0.0F, 0.0F, epr);
		auto endpoint = circle(0.0F, 0.0F, this->thickness * 1.618F);

		if (rxdiff < 0.0F) {
			this->epcxoff = epr - rxdiff;
		} else {
			this->dxoff += rxdiff;
			this->fxoff -= rxdiff;
		}

		this->density = geometry_freeze(geometry_translate(density, this->dxoff, this->yoff));
		this->flspeed = geometry_freeze(geometry_translate(flspeed, this->fxoff, this->yoff));

		{ // make endpoints
			float fepcx = this->epcxoff;
			float depcx = this->width - this->epcxoff;

			this->endpoint_bases = geometry_freeze(geometry_union(endpoint_base, depcx, this->yoff, endpoint_base, fepcx, this->yoff));
			this->endpoints = geometry_freeze(geometry_union(endpoint, depcx, this->yoff, endpoint, fepcx, this->yoff));
		}
	}

	{ // make dimensions
		TextExtent dte, fte;
		auto unit_font = make_bold_text_format("Cambria", dmetrics.em * 1.5F);
		auto dunit = paragraph(make_text_layout(unitspeak("tpm3"), unit_font), &dte);
		auto funit = paragraph(make_text_layout(unitspeak("mps"), unit_font), &fte);
		
		this->label_rx = this->width - fte.width;
		this->label_by = this->height * 0.2718F;

		this->density_unit = geometry_freeze(geometry_translate(dunit, 0.0F, this->label_by - dte.height));
		this->flspeed_unit = geometry_freeze(geometry_translate(funit, label_rx, this->label_by - fte.height));

		this->value_font = make_bold_text_format("Cambria Math", dmetrics.em * 2.0F);
		this->set_values(this->density_min, this->flspeed_min, true);
	}
}

void DensitySpeedmeterlet::fill_extent(float x, float y, float* w, float* h) {
	SET_VALUES(w, this->width, h, this->height);
}

void DensitySpeedmeterlet::set_values(double density, double flspeed, bool force) {
	this->set_density(density, force);
	this->set_flspeed(flspeed, force);
}

void DensitySpeedmeterlet::set_density(double density, bool force) {
	float depr = this->width - this->epcxoff;
	float r = this->radius;
	float px, py;

	if (force || (this->_density != density)) {
		this->_density = density;

		dfmeter_point(r, density, this->density_min, this->density_max, this->density_start, this->density_end, &px, &py);
		this->density_pointer = geometry_freeze(line(depr, this->yoff, this->dxoff + px, this->yoff + py, this->thickness));
		this->density_value = make_text_layout(flstring(density, 2U), this->value_font);

		this->notify_updated();
	}
}

void DensitySpeedmeterlet::set_flspeed(double flspeed, bool force) {
	float r = this->radius;
	float px, py;

	if (force || (this->_flspeed != flspeed)) {
		this->_flspeed = flspeed;

		dfmeter_point(r, flspeed, this->flspeed_min, this->flspeed_max, this->flspeed_start, this->flspeed_end, &px, &py);
		this->flspeed_pointer = geometry_freeze(line(this->epcxoff, this->yoff, this->fxoff + px, this->yoff + py, this->thickness));
		this->flspeed_value = make_text_layout(flstring(flspeed, 2U), this->value_font);

		this->notify_updated();
	}
}

void DensitySpeedmeterlet::draw(CanvasDrawingSession^ ds, float x, float y, float Width, float Height) {
	Rect dbox = this->density_value->LayoutBounds;
	Rect fbox = this->flspeed_value->LayoutBounds;
	float box_rx = x + this->label_rx;
	float box_by = y + this->label_by;

	ds->DrawCachedGeometry(this->endpoint_bases, x, y, this->base_color);
	ds->DrawCachedGeometry(this->endpoints, x, y, this->pointer_color);
	
	ds->DrawCachedGeometry(this->density, x, y, this->density_color);
	ds->DrawTextLayout(this->density_value, x, box_by - dbox.Height, this->pointer_color);
	ds->DrawCachedGeometry(this->density_unit, x + dbox.Width, y, this->density_color);

	ds->DrawCachedGeometry(this->flspeed, x, y, this->flspeed_color);
	ds->DrawTextLayout(this->flspeed_value, box_rx - fbox.Width, box_by - fbox.Height, this->pointer_color);
	ds->DrawCachedGeometry(this->flspeed_unit, x, y, this->flspeed_color);
	
	ds->DrawCachedGeometry(this->density_pointer, x, y, this->pointer_color);
	ds->DrawCachedGeometry(this->flspeed_pointer, x, y, this->pointer_color);
}
