#include "valvelet.hpp"

#include "polar_shape.hpp"
#include "paint.hpp"
#include "geometry.hpp"
#include "brushes.hxx"

using namespace WarGrey::SCADA;

using namespace Windows::UI;

using namespace Microsoft::Graphics::Canvas;
using namespace Microsoft::Graphics::Canvas::Text;
using namespace Microsoft::Graphics::Canvas::Brushes;

static float default_thickness = 1.5F;
static double dynamic_mask_interval = 1.0 / 8.0;

static ValveState default_pump_state = ValveState::Manual;
static CanvasSolidColorBrush^ default_sketeton_color = Colours::DarkGray;

ValveStyle WarGrey::SCADA::make_default_valve_style(ValveState state) {
	ValveStyle s;

	s.skeleton_color = default_sketeton_color;

	switch (state) {
	case ValveState::Manual: s.mask_color = Colours::Teal; s.handler_color = default_sketeton_color; break;
	case ValveState::Open: s.body_color = Colours::Green; break;
	case ValveState::Opening: s.mask_color = Colours::Green; break;
	case ValveState::ConditionalOpen: s.skeleton_color = Colours::Cyan; s.mask_color = Colours::ForestGreen; break;
	case ValveState::Unopenable: s.skeleton_color = Colours::Red; s.mask_color = Colours::Green; break;
	case ValveState::Closed: s.body_color = Colours::LightGray; break;
	case ValveState::Closing: s.mask_color = Colours::DarkGray; break;
	case ValveState::ConditionalClose: s.skeleton_color = Colours::Cyan; s.mask_color = Colours::DimGray; break;
	case ValveState::Unclosable: s.skeleton_color = Colours::Red; s.mask_color = Colours::DarkGray; break;
	case ValveState::FalseOpen: s.border_color = Colours::Red; s.body_color = Colours::ForestGreen; break;
	case ValveState::FalseClosed: s.border_color = Colours::Red; s.body_color = Colours::DimGray; break;
	}

	return s;
}

/*************************************************************************************************/
Valvelet::Valvelet(float radius, double degrees) : Valvelet(default_pump_state, radius, degrees) {}

Valvelet::Valvelet(ValveState default_state, float radius, double degrees)
	: IStatelet(default_state, &make_default_valve_style), size(radius * 2.0F), degrees(degrees) {
	
	this->fradius = radius - default_thickness;
	this->sgradius = this->fradius - default_thickness * 4.0F;
	this->on_state_change(default_state);
}

void Valvelet::construct() {
	float handle_length = this->sgradius * 0.618F;
	auto handler_axis = polar_axis(handle_length, this->degrees);
	auto handler_pole = polar_pole(handle_length, this->degrees, handle_length * 0.1618F);

	this->frame = polar_rectangle(this->fradius, this->degrees);
	this->handler = geometry_union(handler_axis, handler_pole);
	this->skeleton = polar_sandglass(this->sgradius, this->degrees);
	this->body = geometry_freeze(this->skeleton);
}

void Valvelet::update(long long count, long long interval, long long uptime) {
	switch (this->get_state()) {
	case ValveState::Opening: {
		this->mask_percentage
			= ((this->mask_percentage < 0.0) || (this->mask_percentage >= 1.0))
			? 0.0
			: this->mask_percentage + dynamic_mask_interval;

		this->mask = polar_masked_sandglass(this->sgradius, this->degrees, this->mask_percentage);
	} break;
	case ValveState::Closing: {
		this->mask_percentage
			= ((this->mask_percentage <= 0.0) || (this->mask_percentage > 1.0))
			? 1.0
			: this->mask_percentage - dynamic_mask_interval;

		this->mask = polar_masked_sandglass(this->sgradius, this->degrees, -this->mask_percentage);
	} break;
	}
}

void Valvelet::on_state_change(ValveState state) {
	switch (state) {
	case ValveState::Unopenable: {
		if (this->bottom_up_mask == nullptr) {
			this->bottom_up_mask = polar_masked_sandglass(this->sgradius, this->degrees, 0.80);
		}
		this->mask = this->bottom_up_mask;
	} break;
	case ValveState::Unclosable: case ValveState::Manual: {
		if (this->top_down_mask == nullptr) {
			this->top_down_mask = polar_masked_sandglass(this->sgradius, this->degrees, -0.80);
		}
		this->mask = this->top_down_mask;
	} break;
	case ValveState::ConditionalOpen: {
		if (this->bottom_up_conditional_mask == nullptr) {
			this->bottom_up_conditional_mask = polar_masked_sandglass(this->sgradius, this->degrees, 0.70);
		}
		this->mask = this->bottom_up_conditional_mask;
	} break;
	case ValveState::ConditionalClose: {
		if (this->top_down_conditional_mask == nullptr) {
			this->top_down_conditional_mask = polar_masked_sandglass(this->sgradius, this->degrees, -0.70);
		}
		this->mask = this->top_down_conditional_mask;
	} break;
	default: {
		this->mask = nullptr;
		this->mask_percentage = -1.0;
	}
	}
}

void Valvelet::fill_extent(float x, float y, float* w, float* h) {
	SET_BOXES(w, h, size);
}

double Valvelet::get_direction_degrees() {
	return this->degrees;
}

void Valvelet::draw(CanvasDrawingSession^ ds, float x, float y, float Width, float Height) {
	const ValveStyle style = this->get_style();
	auto skeleton_color = (style.skeleton_color != nullptr) ? style.skeleton_color : default_sketeton_color;
	auto body_color = (style.body_color != nullptr) ? style.body_color : Colours::Background;

	float radius = this->size * 0.5F - default_thickness;
	float cx = x + radius + default_thickness;
	float cy = y + radius + default_thickness;
	
	if (style.border_color != nullptr) {
		ds->DrawGeometry(this->frame, cx, cy, style.border_color, default_thickness);
	}

	ds->DrawCachedGeometry(this->body, cx, cy, body_color);

	if (style.mask_color != nullptr) {
		auto mask = ((this->mask == nullptr) ? this->skeleton : this->mask);
		
		ds->FillGeometry(mask, cx, cy, style.mask_color);
		ds->DrawGeometry(mask, cx, cy, style.mask_color, default_thickness);
	}

	ds->DrawGeometry(this->skeleton, cx, cy, skeleton_color, default_thickness);

	if (style.handler_color != nullptr) {
		ds->DrawGeometry(this->handler, cx, cy, style.handler_color, default_thickness);
	}
}
