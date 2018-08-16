#include "graphlet/device/tanklet.hpp"

#include "shape.hpp"
#include "paint.hpp"
#include "hatch.hpp"
#include "geometry.hpp"
#include "transformation.hpp"

using namespace WarGrey::SCADA;

using namespace Windows::Foundation;

using namespace Microsoft::Graphics::Canvas;
using namespace Microsoft::Graphics::Canvas::Brushes;
using namespace Microsoft::Graphics::Canvas::Geometry;

static CanvasSolidColorBrush^ tank_default_border_color = Colours::make(0xBBBBBB);
static CanvasSolidColorBrush^ tank_default_fill_color = Colours::make(0x333333);
static unsigned int tank_default_colors[] = { 0x00BFFF, 0xB3F000, 0xFFB03A, 0xFFB03A };
static float tank_default_color_positions[] = { 0.0F, 0.625F, 0.75F, 1.0F };

/*************************************************************************************************/
ITanklet::ITanklet(float width, float height, float thickness) : width(width), height(height), thickness(thickness) {
	if (this->height < 0.0F) {
		this->height *= (-this->width);
	} else if (this->height == 0.0F) {
		this->height = this->width * 0.382F;
	}

	this->float_half_height = this->thickness * 1.618F * 0.75F;
}

void ITanklet::construct() {
	Platform::String^ en_marks[] = { "E", "UL", "L", "N", "H", "F" };
	Platform::String^ zh_marks[] = { "空", "过低", "低", "正常", "高", "满" };
	unsigned int weights[] = { 0, 2, 3, 5, 8, 10 };
	unsigned int connectors[] = { 3, 7 };

	VHatchMarkMetrics imetrics = vhatchmark_metrics(zh_marks, 6U, this->thickness);
	float thickoff = this->thickness * 0.5F;
	float radius = this->thickness * 2.0F;
	float tube_radius = radius * 0.618F;
	
	float tube_width = imetrics.width - imetrics.mark_width;
	float tube_height = this->height - imetrics.hatch_y * 2.0F;
	float tube_thickness = this->thickness * 1.618F;
	auto tube_shape = vrhatch(tube_width, tube_height, connectors, 2U, 10U, tube_thickness);

	float float_radius = tube_radius * 0.618F;
	float float_height = this->float_half_height * 2.0F;
	float float_width = float_height * 0.618F;
	float float_x = (tube_thickness - float_width) * 0.5F + thickoff;
	auto float_body = rounded_rectangle(float_x, 0.0F, float_width, float_height, float_radius, float_radius);
	auto float_centerline = hline(float_x, float_height * 0.5F, float_width, 0.5F);

	float body_x = tube_width + thickoff;
	float body_width = this->width - body_x - imetrics.width - this->thickness * 1.618F;
	float body_height = this->height - this->thickness;
	float ruler_x = this->width - imetrics.width;
	
	this->ruler_em = imetrics.em;
	this->body = rounded_rectangle(body_x, thickoff, body_width, body_height, radius, radius);
	this->tube = geometry_translate(tube_shape, thickoff, imetrics.hatch_y);
	this->ruler = geometry_translate(vrhatchmark(this->height, zh_marks, weights, 6U, 10U, this->thickness), ruler_x);
	this->floating = geometry_freeze(geometry_subtract(float_body, float_centerline));
	
	this->update_liquid_level(0.8F, Colours::make(Colours::DodgerBlue, 0.618));
}

void ITanklet::fill_extent(float x, float y, float* w, float* h) {
	SET_VALUES(w, this->width, h, this->height);
}

void ITanklet::draw(CanvasDrawingSession^ ds, float x, float y, float Width, float Height) {
	ds->FillGeometry(this->body, x, y, tank_default_fill_color);
	ds->DrawGeometry(this->body, x, y, tank_default_border_color, this->thickness);
	
	ds->DrawGeometry(this->tube, x, y, tank_default_border_color, this->thickness);
	ds->FillGeometry(this->tube, x, y, Colours::Background);
	ds->FillGeometry(this->ruler, x, y, tank_default_border_color);

	ds->DrawCachedGeometry(this->floating, x, y + this->liquid_y - this->float_half_height, Colours::Orange);
	ds->DrawCachedGeometry(this->liquid, x, y, this->liquid_color);
	ds->DrawCachedGeometry(this->indicator, x, y, Colours::Orange);
}

void ITanklet::update_liquid_level(float percentage, ICanvasBrush^ color) {
	auto tb = this->tube->ComputeBounds();
	auto rb = this->ruler->ComputeBounds();
	float indicator_y = rb.Y + (rb.Height - ruler_em) * percentage + 1.0F;
	
	this->liquid_color = color;
	this->liquid_y = tb.Y + tb.Height * (1.0F  - percentage);
	this->indicator = geometry_freeze(geometry_intersect(this->ruler, rectangle(rb.X, indicator_y, rb.Width, this->ruler_em)));
	this->liquid = geometry_freeze(geometry_intersect(this->tube, rectangle(tb.X, this->liquid_y, tb.Width, tb.Height)));
}
