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
	: DensityFlowmeterlet(2.0, 10.0, width, height, thickness, step, bcolor, stops) {}

DensityFlowmeterlet::DensityFlowmeterlet(double drange, double frange, float width, float height
	, float thickness, unsigned int step, ICanvasBrush^ bcolor, GradientStops^ stops)
	: density_range(drange), flow_range(frange), width(width), height(height), thickness(thickness), step(step)
	, border_color(bcolor == nullptr ? thermometer_default_border_color : bcolor) {
	
	if (this->height == 0.0F) {
		this->height = this->width;
	}
}

void DensityFlowmeterlet::construct() {
	float rx = this->width * 0.5F;
	float ry = this->height * 0.5F;

	this->density = geometry_freeze(rhatchmark(rx, ry, 180.0, 260.0, 0.0, this->density_range, this->step, this->thickness));
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

	ds->DrawRectangle(x, y, this->width, this->height, Colours::YellowGreen);
}
