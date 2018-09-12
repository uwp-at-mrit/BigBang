#include "graphlet/symbol/door/hatchlet.hpp"

#include "shape.hpp"
#include "polar.hpp"
#include "geometry.hpp"
#include "brushes.hxx"

using namespace WarGrey::SCADA;

using namespace Windows::Foundation;

using namespace Microsoft::Graphics::Canvas;
using namespace Microsoft::Graphics::Canvas::Brushes;
using namespace Microsoft::Graphics::Canvas::Geometry;

static CanvasSolidColorBrush^ hatch_default_color = Colours::SeaGreen;

/*************************************************************************************************/
Hatchlet::Hatchlet(float width, float height, float thickness, CanvasSolidColorBrush^ color)
	: width(width), height(height), thickness(thickness), color((color == nullptr) ? hatch_default_color : color) {
	if (this->height < 0.0F) {
		this->height *= (-this->width);
	} else if (this->height == 0.0F) {
		this->height = this->width;
	}
}

void Hatchlet::construct() {
	float corner_radius = this->thickness;
	float thickoff = this->thickness * 0.5F;
	float border_width = this->width - this->thickness;
	float border_height = this->height - this->thickness;
	float wheel_radius = std::fminf(border_width, border_height) * 0.618F * 0.5F;
	float wheel_thickness = this->thickness * 0.618F;
	auto slash = polar_line(wheel_radius, -45.0, 135.0);
	auto backslash = polar_line(wheel_radius, -135.0, 45.0);
	
	this->wheel_border = geometry_draft(circle(0.0F, 0.0F, wheel_radius), wheel_thickness);
	this->wheel_handler = geometry_draft(geometry_union(slash, backslash), wheel_thickness);
	this->border = rounded_rectangle(thickoff, thickoff, border_width, border_height, corner_radius, corner_radius);
}

void Hatchlet::fill_extent(float x, float y, float* w, float* h) {
	SET_VALUES(w, this->width, h, this->height);
}

void Hatchlet::draw(CanvasDrawingSession^ ds, float x, float y, float Width, float Height) {
	float cx = x + this->width * 0.5F;
	float cy = y + this->height * 0.5F;

	ds->FillGeometry(this->border, x, y, Colours::Background);
	ds->DrawGeometry(this->border, x, y, this->color, this->thickness);
	ds->DrawCachedGeometry(this->wheel_handler, cx, cy, this->color);
	ds->DrawCachedGeometry(this->wheel_border, cx, cy, this->color);
}
