#include "graphlet/dashboard/densityflowmeterlet.hpp"

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

static CanvasSolidColorBrush^ thermometer_default_border_color = Colours::make(0xBBBBBB);
static unsigned int thermometer_default_colors[] = { 0x1E90FF, 0xB3F000, 0xFFB03A, 0xFFB03A };
static float thermometer_default_color_positions[] = { 0.0F, 0.625F, 0.75F, 1.0F };

/*************************************************************************************************/
DensityFlowmeterlet::DensityFlowmeterlet(float width, float height, float thickness
	, unsigned int step, ICanvasBrush^ bcolor, GradientStops^ stops)
	: DensityFlowmeterlet(1.0, 2.0, 0.0, 10.0, width, height, thickness, step, bcolor, stops) {}

DensityFlowmeterlet::DensityFlowmeterlet(double dmin, double dmax, double fmin, double fmax, float width, float height
	, float thickness, unsigned int step, ICanvasBrush^ bcolor, GradientStops^ stops)
	: density_min(dmin), density_max(dmax), flspeed_min(fmin), flspeed_max(fmax)
	, width(width), height(height), thickness(thickness), step(step)
	, border_color(bcolor == nullptr ? thermometer_default_border_color : bcolor) {
	
	if (this->height == 0.0F) {
		this->height = this->width;
	}
}

void DensityFlowmeterlet::construct() {
	float rx = this->width * 0.8F;
	float ry = this->height * 0.8F;

	this->density = geometry_freeze(rhatchmark(rx, 190.0, 260.0, this->flspeed_min, this->flspeed_max, this->step, this->thickness));
	this->flspeed = geometry_freeze(rhatchmark(rx, -10.0, -80.0, this->density_min, this->density_max, this->step, this->thickness));
}

void DensityFlowmeterlet::fill_extent(float x, float y, float* w, float* h) {
	SET_VALUES(w, this->width, h, this->height);
}

/*
void DensityFlowmeterlet::on_value_changed(double v) {
	float mercury_width, mercury_height;
	double p = this->get_percentage();
	
	this->fill_mercury_extent(p, &this->mercury_x, &this->mercury_y, &mercury_width, &mercury_height);
	this->mercury = geometry_freeze(make_thermometer_mercury(mercury_width, mercury_height));
	this->mercury_color = make_solid_brush(gradient_discrete_color(this->colors, this->get_percentage()));
}
*/

void DensityFlowmeterlet::draw(CanvasDrawingSession^ ds, float x, float y, float Width, float Height) {
	//ds->DrawCachedGeometry(this->mercury, x + this->mercury_x, y + this->mercury_y, this->mercury_color);
	//ds->DrawCachedGeometry(this->skeleton, x, y, this->border_color);

	ds->DrawCachedGeometry(this->density, x + this->width * 0.9F, y + this->height * 0.9F, Colours::Crimson);
	ds->DrawCachedGeometry(this->flspeed, x + this->width * 0.1F, y + this->height * 0.9F, Colours::DodgerBlue);

	ds->DrawRectangle(x, y, this->width, this->height, Colours::YellowGreen);
}
