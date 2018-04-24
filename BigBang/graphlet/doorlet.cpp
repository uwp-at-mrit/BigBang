#include "doorlet.hpp"

#include "polar.hpp"
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

static DoorState default_door_state = DoorState::Closed;
static CanvasSolidColorBrush^ default_sketeton_color = Colours::DarkGray;

DoorStyle WarGrey::SCADA::make_default_door_style(DoorState state) {
	DoorStyle s;

	s.skeleton_color = default_sketeton_color;

	switch (state) {
	case DoorState::Open: s.body_color = Colours::Green; break;
	case DoorState::Opening: s.mask_color = Colours::Green; break;
	case DoorState::Closed: s.body_color = Colours::LightGray; break;
	case DoorState::Closing: s.mask_color = Colours::DarkGray; break;
	}

	return s;
}

/*************************************************************************************************/
DumpDoorlet::DumpDoorlet(float radius, double degrees) : DumpDoorlet(default_door_state, radius, degrees) {}

DumpDoorlet::DumpDoorlet(DoorState default_state, float radius, double degrees)
	: IStatelet(default_state, &make_default_door_style), size(radius * 2.0F), degrees(degrees) {
	this->fradius = radius;
	this->sgradius = this->fradius - default_thickness * 2.0F;
	this->on_state_change(default_state);
}

void DumpDoorlet::construct() {
	float handle_length = this->sgradius * 0.618F;
	auto handler_axis = polar_axis(handle_length, this->degrees);
	auto handler_pole = polar_pole(handle_length, this->degrees, handle_length * 0.1618F);

	this->frame = polar_rectangle(this->fradius, 60.0, this->degrees);
	this->handler = geometry_union(handler_axis, handler_pole);
	this->skeleton = polar_sandglass(this->sgradius, this->degrees);
	this->body = geometry_freeze(this->skeleton);
}

void DumpDoorlet::update(long long count, long long interval, long long uptime) {
	switch (this->get_state()) {
	case DoorState::Opening: {
		this->mask_percentage
			= ((this->mask_percentage < 0.0) || (this->mask_percentage >= 1.0))
			? 0.0
			: this->mask_percentage + dynamic_mask_interval;

		this->mask = polar_masked_sandglass(this->sgradius, this->degrees, this->mask_percentage);
	} break;
	case DoorState::Closing: {
		this->mask_percentage
			= ((this->mask_percentage <= 0.0) || (this->mask_percentage > 1.0))
			? 1.0
			: this->mask_percentage - dynamic_mask_interval;

		this->mask = polar_masked_sandglass(this->sgradius, this->degrees, -this->mask_percentage);
	} break;
	}
}

void DumpDoorlet::on_state_change(DoorState state) {
	switch (state) {
	default: {
		this->mask = nullptr;
		this->mask_percentage = -1.0;
	}
	}
}

void DumpDoorlet::fill_extent(float x, float y, float* w, float* h) {
	SET_BOXES(w, h, size);
}

double DumpDoorlet::get_direction_degrees() {
	return this->degrees;
}

void DumpDoorlet::draw(CanvasDrawingSession^ ds, float x, float y, float Width, float Height) {
	const DoorStyle style = this->get_style();
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
