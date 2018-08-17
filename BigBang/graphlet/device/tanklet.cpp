#include "graphlet/device/tanklet.hpp"

#include "shape.hpp"
#include "geometry.hpp"

using namespace WarGrey::SCADA;

using namespace Windows::Foundation;

using namespace Microsoft::Graphics::Canvas;
using namespace Microsoft::Graphics::Canvas::Brushes;
using namespace Microsoft::Graphics::Canvas::Geometry;

/*************************************************************************************************/
void WarGrey::SCADA::default_mark_weights(float* weights, unsigned int count) {
	 if (count == 1) {
		 weights[0] = 0.5F;
	 } else {
		 float flcount = float(count - 1);

		 for (unsigned int idx = 0; idx < count; idx ++) {
			 weights[idx] = float(idx) / flcount;
		 }
	 }
}

void WarGrey::SCADA::default_liquid_colors(ICanvasBrush^ lcolors[], unsigned int count) {
	static ICanvasBrush^ default_liquid_color = Colours::make(Colours::DarkOrange, 0.618);
	
	for (unsigned int idx = 0; idx < count; idx++) {
		lcolors[idx] = default_liquid_color;
	}
}

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
	VHatchMarkMetrics imetrics;
	CanvasGeometry^ ruler0 = this->make_ruler(this->height, this->thickness, &imetrics);

	float connector_weights[] = { 0.3F, 0.7F };
	float thickoff = this->thickness * 0.5F;
	float radius = this->thickness * 2.0F;
	float tube_radius = radius * 0.618F;
	
	float tube_height = this->height - imetrics.hatch_y * 2.0F;
	float tube_thickness = this->thickness * 1.618F;
	float tube_width = tube_thickness * 2.718F;
	auto tube_shape = vrhatch(tube_width, tube_height, connector_weights, 2U, tube_thickness);

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
	this->ruler = geometry_translate(ruler0, ruler_x);
	this->floating = geometry_freeze(geometry_subtract(float_body, float_centerline));
}

void ITanklet::fill_extent(float x, float y, float* w, float* h) {
	SET_VALUES(w, this->width, h, this->height);
}

void ITanklet::draw(CanvasDrawingSession^ ds, float x, float y, float Width, float Height) {
	if (this->style != nullptr) {
		ds->FillGeometry(this->body, x, y, this->style->body_color);
		ds->DrawGeometry(this->body, x, y, this->style->border_color, this->thickness);

		ds->DrawGeometry(this->tube, x, y, this->style->border_color, this->thickness);
		ds->FillGeometry(this->tube, x, y, Colours::Background);
		ds->FillGeometry(this->ruler, x, y, this->style->border_color);

		ds->DrawCachedGeometry(this->floating, x, y + this->float_y, this->style->indicator_color);
		ds->DrawCachedGeometry(this->liquid, x, y, this->style->liquid_color);
		ds->DrawCachedGeometry(this->indicator, x, y, this->style->indicator_color);
	}
}

void ITanklet::apply_style(TankStyle* style) {
	if (this->tube != nullptr) {
		Rect tbox = this->tube->ComputeBounds();
		Rect rbox = this->ruler->ComputeBounds();
		float indicator_y = rbox.Y + (rbox.Height - ruler_em) * style->mark_weight - 1.0F;
		float liquid_y = tbox.Y + tbox.Height * (1.0F - style->mark_weight);
		auto indicator_box = rectangle(rbox.X, indicator_y, rbox.Width, this->ruler_em + 3.0F);
		auto liquid_box = rectangle(tbox.X, liquid_y, tbox.Width, tbox.Height);

		this->style = style;
		this->float_y = liquid_y - this->float_half_height;
		this->indicator = geometry_freeze(geometry_intersect(this->ruler, indicator_box));
		this->liquid = geometry_freeze(geometry_intersect(this->tube, liquid_box));
	}
}

void ITanklet::prepare_style(TankStyle& style, unsigned int idx, float weights[], ICanvasBrush^ liquid_colors[]) {
	static ICanvasBrush^ default_liquid_color = Colours::make(Colours::Gold, 0.618);

	CAS_SLOT(style.border_color, Colours::make(0xBBBBBB));
	CAS_SLOT(style.body_color, Colours::make(0x333333));
	CAS_SLOT(style.indicator_color, Colours::Crimson);
	CAS_SLOT(style.liquid_color, ((liquid_colors[idx] == nullptr) ? default_liquid_color : liquid_colors[idx]));

	if ((style.mark_weight < 0.0F) || (style.mark_weight > 1.0F)) {
		style.mark_weight = weights[idx];
	}
}
