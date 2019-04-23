#include "graphlet/device/tanklet.hpp"

#include "datum/flonum.hpp"

#include "shape.hpp"
#include "geometry.hpp"

using namespace WarGrey::SCADA;

using namespace Windows::Foundation;

using namespace Microsoft::Graphics::Canvas;
using namespace Microsoft::Graphics::Canvas::Brushes;
using namespace Microsoft::Graphics::Canvas::Geometry;

static CanvasSolidColorBrush^ tank_default_body_color = Colours::make(0x333333);
static CanvasSolidColorBrush^ tank_default_ruler_color = Colours::make(0x666666);
static CanvasSolidColorBrush^ tank_default_border_color = Colours::make(0xBBBBBB);
static CanvasSolidColorBrush^ tank_default_liquid_color = Colours::make(Colours::DarkOrange, 0.618);

/*************************************************************************************************/
ITanklet::ITanklet(float width, float height, float thickness) : width(width), height(height), thickness(thickness) {
	if (this->height == 0.0F) {
		this->height = this->width * 0.382F;
	}

	this->float_half_height = this->thickness * 1.618F * 0.75F;
}

void ITanklet::construct() {
	VHatchMarkMetrics imetrics;
	CanvasGeometry^ ruler0 = this->make_ruler(this->height, this->thickness, &imetrics);

	double connector_weights[] = { 0.3, 0.7 };
	float thickoff = this->thickness * 0.5F;
	float radius = this->thickness * 2.0F;
	float tube_radius = radius * 0.618F;

	float float_radius = tube_radius * 0.618F;
	float float_height = this->float_half_height * 2.0F;
	float float_width = float_height * 0.618F;

	float tube_height = this->height - imetrics.hatch_y * 2.0F;
	float tube_thickness = float_width * 1.2F;
	float tube_width = tube_thickness * 2.718F;
	auto tube_shape = vrhatch(tube_width, tube_height, connector_weights, 2U, tube_thickness);

	float float_x = (tube_thickness - float_width) * 0.5F + thickoff;
	auto float_body = rounded_rectangle(float_x, 0.0F, float_width, float_height, float_radius, float_radius);
	auto float_centerline = hline(float_x, float_height * 0.5F, float_width, 0.5F);

	float body_x = tube_width + thickoff;
	float ruler_x = this->width - imetrics.width - this->thickness;
	float body_width = ruler_x - body_x - this->thickness * 1.618F;
	float body_height = this->height - this->thickness;

	this->ruler_em = imetrics.em;

	this->body = rounded_rectangle(body_x, thickoff, body_width, body_height, radius, radius);
	this->tube = geometry_translate(tube_shape, thickoff, imetrics.hatch_y);
	this->ruler = geometry_translate(ruler0, ruler_x);
	this->floating = geometry_freeze(geometry_subtract(float_body, float_centerline));

	this->update_level(0.0, 0.0, 1.0);
}

void ITanklet::fill_extent(float x, float y, float* w, float* h) {
	SET_VALUES(w, this->width, h, this->height);
}

void ITanklet::update_level(double value, double vmin, double vmax) {
	Rect tbox = this->tube->ComputeBounds();
	Rect rbox = this->ruler->ComputeBounds();
	double percentage = (value - vmin) / (vmax - vmin);
	float indicator_y = rbox.Y + (rbox.Height - ruler_em) * float(1.0 - percentage) - 1.0F;
	float liquid_y = tbox.Y + tbox.Height * float(1.0 - percentage);
	auto indicator_box = rectangle(rbox.X, indicator_y, rbox.Width, this->ruler_em + 3.0F);
	auto liquid_box = rectangle(tbox.X, liquid_y, tbox.Width, tbox.Height);

	this->float_y = liquid_y - this->float_half_height;
	this->indicator = geometry_freeze(geometry_intersect(this->ruler, indicator_box));
	this->liquid = geometry_freeze(geometry_intersect(this->tube, liquid_box));
}

void ITanklet::draw(CanvasDrawingSession^ ds, float x, float y, float Width, float Height) {
	TankStyle* style = this->get_tank_style();

	ds->FillGeometry(this->body, x, y, style->body_color);
	ds->DrawGeometry(this->body, x, y, style->border_color, this->thickness);

	ds->DrawGeometry(this->tube, x, y, style->border_color, this->thickness);
	ds->FillGeometry(this->tube, x, y, Colours::Background);
	ds->FillGeometry(this->ruler, x, y, style->ruler_color);

	ds->DrawCachedGeometry(this->floating, x, y + this->float_y, style->indicator_color);
	ds->DrawCachedGeometry(this->liquid, x, y, style->liquid_color);
	ds->DrawCachedGeometry(this->indicator, x, y, style->indicator_color);
}

/*************************************************************************************************/
Tanklet::Tanklet(double range, float width, float height, unsigned int step, float thickness, unsigned int precision)
	: Tanklet(0.0, range, width, height, step, thickness, precision) {}

Tanklet::Tanklet(TankState default_state, double range, float width, float height
	, unsigned int step, float thickness, unsigned int precision)
	: Tanklet(default_state, 0.0, range, width, height, step, thickness, precision) {}

Tanklet::Tanklet(double vmin, double vmax, float width, float height, unsigned int step, float thickness, unsigned int precision)
	: Tanklet(TankState::Normal, vmin, vmax, width, height, step, thickness, precision) {}

Tanklet::Tanklet(TankState default_state, double vmin, double vmax, float width, float height
	, unsigned int step, float thickness, unsigned int precision)
	: ITanklet(width, height, thickness), IStatelet(default_state), IRangelet(vmin, vmax)
	, step(step), precision(precision) {}

void Tanklet::prepare_style(TankState state, TankStyle& style) {
	switch (state) {
	case TankState::Full: case TankState::Empty: {
		CAS_SLOT(style.indicator_color, Colours::Firebrick);
	}; break;
	case TankState::UltraHigh: case TankState::UltraLow: {
		CAS_SLOT(style.indicator_color, Colours::Red);
	}; break;
	case TankState::High: case TankState::Low: {
		CAS_SLOT(style.indicator_color, Colours::Yellow);
	}; break;
	}

	CAS_SLOT(style.body_color, tank_default_body_color);
	CAS_SLOT(style.border_color, tank_default_border_color);
	CAS_SLOT(style.ruler_color, style.border_color);
	CAS_SLOT(style.liquid_color, tank_default_liquid_color);
	CAS_SLOT(style.indicator_color, Colours::Green);
}

void Tanklet::on_value_changed(double v) {
	ITanklet::update_level(v, this->vmin, this->vmax);
}

TankStyle* Tanklet::get_tank_style() {
	return &(this->get_style());
}

CanvasGeometry^ Tanklet::make_ruler(float height, float thickness, VHatchMarkMetrics* metrics) {
	return vrhatchmark(height, this->vmin, this->vmax, this->step, thickness, metrics, this->precision);
}

/*************************************************************************************************/
IStateTanklet::IStateTanklet(float width, float height, float thickness) : ITanklet(width, height, thickness) {}

void IStateTanklet::prepare_style(StateTankStyle& style, unsigned int idx, unsigned int count) {
	CAS_SLOT(style.body_color, tank_default_body_color);
	CAS_SLOT(style.ruler_color, tank_default_ruler_color);
	CAS_SLOT(style.border_color, tank_default_border_color);
	CAS_SLOT(style.liquid_color, tank_default_liquid_color);

	if ((style.mark_weight < 0.0) || (style.mark_weight > 1.0)) {
		style.mark_weight = ((count == 1) ? 0.5 : double(idx) / double(count - 1));
	}

	if (style.indicator_color == nullptr) {
		unsigned int fxweight = (unsigned int)flfloor(style.mark_weight * 10.0);
		unsigned int weight = ((fxweight > 5) ? (10 - fxweight) : fxweight);

		if (weight >= 4) {
			style.indicator_color = Colours::Green;
		} else if (weight >= 2) {
			style.indicator_color = Colours::Yellow;
		} else {
			style.indicator_color = Colours::Crimson;
		}
	}
}
