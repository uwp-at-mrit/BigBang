#include "pumplet.hpp"

#include "shape.hpp"
#include "paint.hpp"
#include "geometry.hpp"

using namespace WarGrey::SCADA;

using namespace Windows::UI;

using namespace Microsoft::Graphics::Canvas;
using namespace Microsoft::Graphics::Canvas::Text;
using namespace Microsoft::Graphics::Canvas::Brushes;

static double dynamic_mask_interval = 1.0 / 8.0;
static PumpState default_pump_state = PumpState::Stopped;
static CanvasSolidColorBrush^ default_body_color = darkgray_brush();
static CanvasSolidColorBrush^ default_border_color = whitesmoke_brush();

PumpStyle WarGrey::SCADA::make_default_pump_style(PumpState state) {
	PumpStyle s;

	s.border_color = default_border_color;
	s.body_color = default_body_color;

	switch (state) {
	case PumpState::Running: s.body_color = green_brush(); break;
	case PumpState::Starting: s.body_color = dimgray_brush(); s.mask_color = green_brush(); break;
	case PumpState::Unstartable: s.body_color = dimgray_brush(); s.mask_color = green_brush(); break;
	case PumpState::Remote: s.border_color = cyan_brush(); break;
	case PumpState::Stopped: break; // this is the default to draw
	case PumpState::Stopping: s.mask_color = forest_brush(); break;
	case PumpState::Unstoppable: s.mask_color = forest_brush(); break;
	case PumpState::Ready: s.skeleton_color = cyan_brush(); break;
	}

	return s;
}

/*************************************************************************************************/
Pumplet::Pumplet(float radius, double degrees) : Pumplet(default_pump_state, radius, degrees) {}

Pumplet::Pumplet(PumpState default_state, float radius, double degrees)
	: IStatelet(default_state, &make_default_pump_style), size(radius * 2.0F), degrees(degrees), thickness(2.0F) {
	
	this->tradius = radius - this->thickness * 2.0F;
	this->on_state_change(default_state);
}

void Pumplet::construct() {
	this->skeleton = triangle(this->tradius, this->degrees);
	this->body = geometry_freeze(this->skeleton);
}

void Pumplet::update(long long count, long long interval, long long uptime) {
	switch (this->get_state()) {
	case PumpState::Starting: {
		this->mask_percentage
			= ((this->mask_percentage < 0.0) || (this->mask_percentage >= 1.0))
			? 0.0
			: this->mask_percentage + dynamic_mask_interval;

		this->mask = trapezoid(this->tradius, this->degrees, this->mask_percentage);
	} break;
	case PumpState::Stopping: {
		this->mask_percentage
			= ((this->mask_percentage <= 0.0) || (this->mask_percentage > 1.0))
			? 1.0
			: this->mask_percentage - dynamic_mask_interval;

		this->mask = trapezoid(this->tradius, this->degrees, this->mask_percentage);
	} break;
	}
}

void Pumplet::on_state_change(PumpState state) {
	switch (state) {
	case PumpState::Unstartable: {
		if (this->unstartable_mask == nullptr) {
			this->unstartable_mask = trapezoid(this->tradius, this->degrees, 0.382);
		}
		this->mask = this->unstartable_mask;
	} break;
	case PumpState::Unstoppable: {
		if (this->unstoppable_mask == nullptr) {
			this->unstoppable_mask = trapezoid(this->tradius, this->degrees, 0.618);
		}
		this->mask = this->unstoppable_mask;
	} break;
	default: {
		this->mask = nullptr;
		this->mask_percentage = -1.0;
	}
	}
}

void Pumplet::fill_extent(float x, float y, float* w, float* h) {
	SET_BOXES(w, h, size);
}

double Pumplet::get_direction_degrees() {
	return this->degrees;
}

void Pumplet::draw(CanvasDrawingSession^ ds, float x, float y, float Width, float Height) {
	const PumpStyle style = this->get_style();
	auto body_color = (style.body_color != nullptr) ? style.body_color : default_body_color;
	auto skeleton_color = (style.skeleton_color != nullptr) ? style.skeleton_color : body_color;
	auto border_color = (style.border_color != nullptr) ? style.border_color : default_border_color;

	float radius = this->size * 0.5F - this->thickness;
	float cx = x + radius + this->thickness;
	float cy = y + radius + this->thickness;
	float bx = cx - radius + this->thickness;
	float by = cy - radius + this->thickness;

	ds->FillCircle(cx, cy, radius, system_background_brush());
	ds->DrawCircle(cx, cy, radius, border_color, this->thickness);
	ds->DrawCachedGeometry(this->body, bx, by, body_color);
	ds->DrawGeometry(this->skeleton, bx, by, skeleton_color, this->thickness);

	if (style.mask_color != nullptr) {
		auto mask = ((this->mask == nullptr) ? this->skeleton : this->mask);
		
		ds->FillGeometry(mask, bx, by, style.mask_color);
		ds->DrawGeometry(mask, bx, by, style.mask_color, this->thickness);
	}
}
