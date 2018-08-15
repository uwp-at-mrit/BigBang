#include "graphlet/device/tanklet.hpp"

#include "shape.hpp"
#include "paint.hpp"
#include "hatch.hpp"
#include "geometry.hpp"
#include "colorspace.hpp"
#include "transformation.hpp"

using namespace WarGrey::SCADA;

using namespace Windows::Foundation;

using namespace Microsoft::Graphics::Canvas;
using namespace Microsoft::Graphics::Canvas::Brushes;
using namespace Microsoft::Graphics::Canvas::Geometry;

static CanvasSolidColorBrush^ tank_default_border_color = Colours::make(0xBBBBBB);
static CanvasSolidColorBrush^ tank_default_fill_color = Colours::make(0x333333);
static CanvasSolidColorBrush^ tank_default_heater_color = Colours::make(0x663366);
static unsigned int tank_default_colors[] = { 0x00BFFF, 0xB3F000, 0xFFB03A, 0xFFB03A };
static float tank_default_color_positions[] = { 0.0F, 0.625F, 0.75F, 1.0F };

/*************************************************************************************************/
Tanklet::Tanklet(float width, float height, float thickness
	, CanvasSolidColorBrush^ bcolor, CanvasSolidColorBrush^ color, CanvasSolidColorBrush^ hcolor, GradientStops^ colors)
	: Tanklet(FitPosition::Left, width, height, thickness, bcolor, color, hcolor, colors) {}

Tanklet::Tanklet(FitPosition position, float width, float height, float thickness
	, CanvasSolidColorBrush^ bcolor, CanvasSolidColorBrush^ color, CanvasSolidColorBrush^ hcolor, GradientStops^ colors)
	: IRangelet(0.0F, 1.0F), width(width), height(height), thickness(thickness), mark_position(position)
	, border_color((bcolor == nullptr) ? tank_default_border_color : bcolor)
	, fill_color((color == nullptr) ? tank_default_fill_color : color)
	, heater_color((hcolor == nullptr) ? tank_default_heater_color : hcolor) {
	
	if (this->height < 0.0F) {
		this->height *= (-this->width);
	} else if (this->height == 0.0F) {
		this->height = this->width * 0.382F;
	}

	this->colors = ((colors == nullptr) ? make_gradient_stops(tank_default_colors, tank_default_color_positions) : colors);
}

void Tanklet::construct() {
	float offset = this->thickness * 0.5F;
	float body_width = this->width - this->thickness;
	float body_height = this->height - this->thickness;
	float xradius = this->width * 0.10F;
	float yradius = this->height * 0.25F;
	float heater_length = this->width - xradius * 2.0F;
	float heater_y = body_height + offset;
	auto border = rounded_rectangle(offset, offset, body_width, body_height, xradius, yradius);
	auto mask = rectangle(offset, offset, xradius - this->thickness, body_height);

	this->body = geometry_subtract(border, mask);
	this->heater = geometry_freeze(hline(xradius, heater_y, heater_length, this->thickness));

	this->on_value_changed(0.0F);
}

void Tanklet::fill_extent(float x, float y, float* w, float* h) {
	SET_VALUES(w, this->width, h, this->height);
}

void Tanklet::on_value_changed(float v) {
	float percentage = this->get_percentage();
	
	this->color = make_solid_brush(gradient_discrete_color(this->colors, percentage));
}

void Tanklet::draw(CanvasDrawingSession^ ds, float x, float y, float Width, float Height) {
	ds->FillGeometry(this->body, x, y, this->fill_color);
	ds->DrawGeometry(this->body, x, y, this->border_color, this->thickness);
	ds->DrawCachedGeometry(this->heater, x, y, this->heater_color);
}
