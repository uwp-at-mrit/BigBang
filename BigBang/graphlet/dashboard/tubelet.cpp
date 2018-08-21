#include "graphlet/dashboard/tubelet.hpp"

#include "shape.hpp"
#include "hatch.hpp"
#include "geometry.hpp"

using namespace WarGrey::SCADA;

using namespace Windows::Foundation;

using namespace Microsoft::Graphics::Canvas;
using namespace Microsoft::Graphics::Canvas::Brushes;
using namespace Microsoft::Graphics::Canvas::Geometry;

static CanvasSolidColorBrush^ tube_default_border_color = Colours::make(0xBBBBBB);
static unsigned int tube_default_colors[] = { 0x00BFFF, 0xB3F000, 0xFFB03A, 0xFFB03A };
static float tube_default_color_positions[] = { 0.0F, 0.625F, 0.75F, 1.0F };

/*************************************************************************************************/
Tubelet::Tubelet(float width, float height, float thickness, CanvasSolidColorBrush^ bcolor, GradientStops^ colors)
	: Tubelet(1.0F, width, height, thickness, bcolor, colors) {}

Tubelet::Tubelet(double range, float width, float height, float thickness
	, CanvasSolidColorBrush^ bcolor, GradientStops^ colors)
	: IRangelet(0.0, range), width(width), height(height), thickness(thickness)
	, border_color((bcolor == nullptr) ? tube_default_border_color : bcolor) {
	if (this->height < 0.0F) {
		this->height *= (-this->width);
	} else if (this->height == 0.0F) {
		this->height = this->width * 6.18F;
	}

	this->colors = ((colors == nullptr) ? make_gradient_stops(tube_default_colors, tube_default_color_positions) : colors);
}

void Tubelet::construct() {
	float corner_radius = this->thickness;
	float thickoff = this->thickness * 0.5F;
	float body_x = thickoff;
	float body_y = thickoff;
	float body_width = this->width - body_x * 2.0F;
	float body_height = this->height - body_y * 2.0F;

	this->body = rounded_rectangle(body_x, body_y, body_width, body_height, corner_radius, corner_radius);

	this->on_value_changed(0.0F);
}

void Tubelet::fill_extent(float x, float y, float* w, float* h) {
	SET_VALUES(w, this->width, h, this->height);
}

void Tubelet::on_value_changed(double v) {
	double percentage = this->get_percentage();
	
	this->color = make_solid_brush(gradient_discrete_color(this->colors, percentage));
	this->liquid = geometry_freeze(this->make_liquid_shape(percentage));
}

void Tubelet::draw(CanvasDrawingSession^ ds, float x, float y, float Width, float Height) {
	ds->FillGeometry(this->body, x, y, Colours::Background);
	ds->DrawCachedGeometry(this->liquid, x, y, this->color);
	ds->DrawGeometry(this->body, x, y, this->border_color, this->thickness);
}

CanvasGeometry^ Tubelet::make_liquid_shape(double percentage) {
	Rect region = this->body->ComputeBounds();
	float hollow_height = region.Height * float(1.0 - percentage);
	
	return geometry_subtract(this->body, rectangle(region.X, region.Y, region.Width, hollow_height));
}
