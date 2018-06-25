#include "graphlet/symbol/doorlet.hpp"

#include "polar.hpp"
#include "paint.hpp"
#include "geometry.hpp"
#include "brushes.hxx"

using namespace WarGrey::SCADA;

using namespace Microsoft::Graphics::Canvas;
using namespace Microsoft::Graphics::Canvas::Brushes;

static float default_thickness = 1.5F;
static double dynamic_mask_interval = 1.0 / 8.0;

/*************************************************************************************************/
DumpDoorlet::DumpDoorlet(float radius, double degrees) : DumpDoorlet(DoorStatus::Closed, radius, degrees) {}

DumpDoorlet::DumpDoorlet(DoorStatus default_state, float radius, double degrees) : ISymbollet(default_state, radius, degrees) {
	this->fradius = radius;
	this->sgradius = this->fradius - default_thickness * 2.0F;
	this->update_status();
}

void DumpDoorlet::construct() {
	float handle_length = this->sgradius * 0.618F;
	auto handler_axis = polar_axis(handle_length, this->degrees);
	auto handler_pole = polar_pole(handle_length, this->degrees, handle_length * 0.1618F);

	this->frame = polar_rectangle(this->fradius, 60.0, this->degrees);
	this->handle = geometry_union(handler_axis, handler_pole);
	this->skeleton = polar_sandglass(this->sgradius, this->degrees);
	this->body = geometry_freeze(this->skeleton);
}

void DumpDoorlet::update(long long count, long long interval, long long uptime) {
	switch (this->get_status()) {
	case DoorStatus::Opening: {
		this->mask_percentage
			= ((this->mask_percentage < 0.0) || (this->mask_percentage >= 1.0))
			? 0.0
			: this->mask_percentage + dynamic_mask_interval;

		this->mask = polar_masked_sandglass(this->sgradius, this->degrees, this->mask_percentage);
		this->notify_updated();
	} break;
	case DoorStatus::Closing: {
		this->mask_percentage
			= ((this->mask_percentage <= 0.0) || (this->mask_percentage > 1.0))
			? 1.0
			: this->mask_percentage - dynamic_mask_interval;

		this->mask = polar_masked_sandglass(this->sgradius, this->degrees, -this->mask_percentage);
		this->notify_updated();
	} break;
	}
}

void DumpDoorlet::on_status_change(DoorStatus state) {
	switch (state) {
	default: {
		this->mask = nullptr;
		this->mask_percentage = -1.0;
	}
	}
}

void DumpDoorlet::prepare_style(DoorStatus state, DoorStyle& s) {
	switch (state) {
	case DoorStatus::Open: CAS_SLOT(s.body_color, Colours::Green); break;
	case DoorStatus::Opening: CAS_SLOT(s.mask_color, Colours::Green); break;
	case DoorStatus::Closed: CAS_SLOT(s.body_color, Colours::LightGray); break;
	case DoorStatus::Closing: CAS_SLOT(s.mask_color, Colours::DarkGray); break;
	}

	CAS_SLOT(s.skeleton_color, Colours::DarkGray);
	CAS_SLOT(s.body_color, Colours::Background);

	// NOTE: The others can be nullptr;
}

void DumpDoorlet::draw(CanvasDrawingSession^ ds, float x, float y, float Width, float Height) {
	const DoorStyle style = this->get_style();
	
	float radius = this->size * 0.5F - default_thickness;
	float cx = x + radius + default_thickness;
	float cy = y + radius + default_thickness;
	
	if (style.border_color != nullptr) {
		ds->DrawGeometry(this->frame, cx, cy, style.border_color, default_thickness);
	}

	ds->DrawCachedGeometry(this->body, cx, cy, style.body_color);

	if (style.mask_color != nullptr) {
		auto mask = ((this->mask == nullptr) ? this->skeleton : this->mask);
		
		ds->FillGeometry(mask, cx, cy, style.mask_color);
		ds->DrawGeometry(mask, cx, cy, style.mask_color, default_thickness);
	}

	ds->DrawGeometry(this->skeleton, cx, cy, style.skeleton_color, default_thickness);

	if (style.handler_color != nullptr) {
		ds->DrawGeometry(this->handle, cx, cy, style.handler_color, default_thickness);
	}
}
