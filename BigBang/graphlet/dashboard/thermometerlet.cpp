#include "graphlet/dashboard/thermometerlet.hpp"

#include "shape.hpp"
#include "paint.hpp"
#include "hatch.hpp"
#include "geometry.hpp"

using namespace WarGrey::SCADA;

using namespace Windows::Foundation;
using namespace Windows::Foundation::Numerics;

using namespace Microsoft::Graphics::Canvas;
using namespace Microsoft::Graphics::Canvas::Brushes;
using namespace Microsoft::Graphics::Canvas::Geometry;

static CanvasSolidColorBrush^ overflowlet_default_border_color = Colours::make(0xBBBBBB);
static unsigned int overflowlet_default_colors[] = { 0x1E90FF, 0xB3F000, 0xFFB03A, 0xFFB03A };
static float overflowlet_default_color_positions[] = { 0.0F, 0.625F, 0.75F, 1.0F };

static CanvasGeometry^ make_thermometer_glass(float width, float height, float thickness) {
	CanvasPathBuilder^ glass = ref new CanvasPathBuilder(CanvasDevice::GetSharedDevice());
	float offset = thickness * 0.5F;
	float bradius = (width - thickness) * 0.5F;
	float bulb_width = bradius * 2.0F;
	float tube_width = bulb_width * 0.618F;
	float tradius = tube_width * 0.5F;
	float tube_lx = (bulb_width - tube_width) * 0.5F + offset;
	float tube_ty = tradius + offset;
	float tube_rx = bulb_width - tube_lx + thickness;
	float tube_by = height - bradius * (std::sinf(std::acosf(tradius / bradius)) + 1.0F) - offset;

	glass->BeginFigure(tube_rx, tube_ty);
	glass->AddArc(float2(tube_lx, tube_ty), tradius, tradius, 0.0F, CanvasSweepDirection::CounterClockwise, CanvasArcSize::Small);
	glass->AddLine(tube_lx, tube_by);
	glass->AddArc(float2(tube_rx, tube_by), bradius, bradius, 0.0F, CanvasSweepDirection::CounterClockwise, CanvasArcSize::Large);
	glass->AddLine(tube_rx, tube_ty);
	glass->EndFigure(CanvasFigureLoop::Closed);

	return geometry_stroke(CanvasGeometry::CreatePath(glass), thickness);
}

static CanvasGeometry^ make_thermometer_mercury(float bulb_width, float height) {
	float bulb_radius = bulb_width * 0.5F;
	float cx = bulb_radius;
	float cy = height - bulb_radius;
	float tube_width = bulb_width * 0.382F;
	float tube_x = (bulb_width - tube_width) * 0.5F;
	float tradius = tube_width * 0.5F;
	
	CanvasGeometry^ bulb = circle(cx, cy, bulb_radius);
	CanvasGeometry^ bulb_hollow = circle(cx, cy, tradius);
	CanvasGeometry^ glass = rounded_rectangle(tube_x, 0.0F, tube_width, cy - tradius, tradius, tradius);

	return geometry_subtract(geometry_union(glass, bulb), bulb_hollow);
}

/*************************************************************************************************/
Thermometerlet::Thermometerlet(float width, float height, float thickness
	, unsigned int step, ICanvasBrush^ bcolor, GradientStops^ stops)
	: Thermometerlet(-30.0, 50.0, width, height, thickness, step, bcolor, stops) {}

Thermometerlet::Thermometerlet(double range, float width, float height, float thickness
	, unsigned int step, ICanvasBrush^ bcolor, GradientStops^ stops)
	: Thermometerlet(0.0, range, width, height, thickness, step, bcolor, stops) {}

Thermometerlet::Thermometerlet(double tmin, double tmax, float width, float height
	, float thickness, unsigned int step, ICanvasBrush^ bcolor, GradientStops^ stops)
	: IRangelet(tmin, tmax), width(std::fabsf(width)), height(height), thickness(thickness), step(step)
	, border_color(bcolor == nullptr ? overflowlet_default_border_color : bcolor), leftward(width > 0.0F) {
	
	if (this->height == 0.0F) {
		this->height = this->width * 2.0F;
	}

	this->colors = ((stops == nullptr) ? make_gradient_stops(overflowlet_default_colors, overflowlet_default_color_positions) : stops);
}

void Thermometerlet::construct() {
	float mercury_lowest, mercury_highest, mercury_height;
	float hatch_thickness = this->thickness * 0.618F;
	VHatchMarkMetrics metrics = vhatchmark_metrics(this->vmin, this->vmax, hatch_thickness);
	
	this->bulb_size = this->width - metrics.width;
	this->fill_mercury_extent(0.0F, nullptr, &mercury_lowest);
	this->fill_mercury_extent(nullptr, &mercury_highest, nullptr, &mercury_height); 
	
	{ // make skeleton
		CanvasGeometry^ hatchmark;
		float hatch_height = (mercury_lowest - mercury_highest) + hatch_thickness + metrics.em;
		CanvasGeometry^ glass = make_thermometer_glass(this->bulb_size, this->height, this->thickness);
		float mark_x = 0.0F;
		
		if (this->leftward) {
			hatchmark = vlhatchmark(hatch_height, this->vmin, this->vmax, this->step, hatch_thickness, &metrics);
			glass = geometry_translate(glass, metrics.width, 0.0F);
		} else {
			hatchmark = vrhatchmark(hatch_height, this->vmin, this->vmax, this->step, hatch_thickness, &metrics);
			mark_x = this->width - metrics.width;
		}
		    
		this->skeleton = geometry_freeze(geometry_union(glass, hatchmark, mark_x, mercury_highest - metrics.hatch_y));
	}

	this->set_value(0.0, true);
}

void Thermometerlet::fill_extent(float x, float y, float* w, float* h) {
	SET_VALUES(w, this->width, h, this->height);
}

void Thermometerlet::fill_mercury_extent(double percentage, float* x, float* y, float *width, float* height) {
	float mercury_width = this->bulb_size * 0.5F;
	float bulb_cx = (this->leftward ? (this->width - mercury_width) : mercury_width);
	float bulb_cy = this->height - mercury_width;
	float mercury_radius = mercury_width * 0.5F;
	float mercury_bulb_bottom = bulb_cy + mercury_radius;
	float mercury_highest = mercury_radius;
	float mercury_lowest = this->height - this->bulb_size;
	float mercury_tube_height = mercury_lowest - mercury_highest;
	float mercury_height = (mercury_bulb_bottom - mercury_lowest) + mercury_tube_height * float(percentage);
	float mercury_x = bulb_cx - mercury_radius;
	float mercury_y = mercury_bulb_bottom - mercury_height;

	SET_VALUES(x, mercury_x, y, mercury_y);
	SET_VALUES(width, mercury_width, height, mercury_height);
}

void Thermometerlet::fill_mercury_extent(float* x, float* y, float *width, float* height) {
	this->fill_mercury_extent(1.0F, x, y, width, height);
}

void Thermometerlet::on_value_changed(double v) {
	float mercury_width, mercury_height;
	double p = this->get_percentage();
	
	this->fill_mercury_extent(p, &this->mercury_x, &this->mercury_y, &mercury_width, &mercury_height);
	this->mercury = geometry_freeze(make_thermometer_mercury(mercury_width, mercury_height));
	this->mercury_color = make_solid_brush(gradient_discrete_color(this->colors, this->get_percentage()));
}

void Thermometerlet::draw(CanvasDrawingSession^ ds, float x, float y, float Width, float Height) {
	ds->DrawCachedGeometry(this->mercury, x + this->mercury_x, y + this->mercury_y, this->mercury_color);
	ds->DrawCachedGeometry(this->skeleton, x, y, this->border_color);
}
