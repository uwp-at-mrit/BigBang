#include "graphlet/symbol/doorlet.hpp"

#include "polar.hpp"
#include "shape.hpp"
#include "brushes.hxx"

using namespace WarGrey::SCADA;

using namespace Microsoft::Graphics::Canvas;
using namespace Microsoft::Graphics::Canvas::Brushes;
using namespace Microsoft::Graphics::Canvas::Geometry;

static float default_thickness = 2.0F;
static float default_alpha_degrees = 45.0;
static double dynamic_mask_interval = 1.0 / 8.0;

/*************************************************************************************************/
BottomDoorlet::BottomDoorlet(float radius, double degrees) : BottomDoorlet(DoorStatus::Closed, radius, degrees) {}

BottomDoorlet::BottomDoorlet(DoorStatus default_state, float radius, double degrees) : ISymbollet(default_state, radius, degrees) {
	this->radius = radius - default_thickness;
}

void BottomDoorlet::update(long long count, long long interval, long long uptime) {
	switch (this->get_status()) {
	case DoorStatus::Opening: {
		this->mask_percentage
			= ((this->mask_percentage < 0.0) || (this->mask_percentage >= 1.0))
			? 0.0
			: this->mask_percentage + dynamic_mask_interval;

		this->flashing = !this->flashing;
		this->make_masked_door_partitions();
		this->notify_updated();
	} break;
	case DoorStatus::Closing: {
		this->mask_percentage
			= ((this->mask_percentage <= 0.0) || (this->mask_percentage > 1.0))
			? 1.0
			: this->mask_percentage - dynamic_mask_interval;

		this->flashing = !this->flashing;
		this->make_masked_door_partitions();
		this->notify_updated();
	} break;
	}
}

void BottomDoorlet::on_status_changed(DoorStatus state) {
	this->flashing = false;

	switch (state) {
	case DoorStatus::Open: {
		this->mask_percentage = 1.0;
		this->make_masked_door_partitions();
		this->notify_updated();
	} break;
	case DoorStatus::Disabled: {
		if (this->disable_line == nullptr) {
			double d0 = this->degrees - 45.0;
			double dn = this->degrees + 135.0;

			this->disable_line = geometry_draft(polar_line(this->radius, d0, dn), default_thickness);
		}
	} // NOTE: there is no `break` here;
	case DoorStatus::Closed: {
		this->mask_percentage = 0.0;
		this->make_masked_door_partitions();
		this->notify_updated();
	} break;
	}
}

void BottomDoorlet::prepare_style(DoorStatus state, DoorStyle& s) {
	switch (state) {
	case DoorStatus::Disabled: {
		CAS_SLOT(s.disable_color, Colours::Firebrick);
	}; break;
	case DoorStatus::Opening: {
		CAS_SLOT(s.border_hlcolor, Colours::Green);
	}; break;
	case DoorStatus::Closing: {
		CAS_SLOT(s.border_hlcolor, Colours::Yellow);
	}; break;
	}

	CAS_SLOT(s.border_color, Colours::ForestGreen);
	CAS_SLOT(s.border_hlcolor, s.border_color);
	CAS_SLOT(s.door_color, Colours::DimGray);
	CAS_SLOT(s.body_color, Colours::DarkKhaki);
	CAS_SLOT(s.skeleton_color, Colours::Black);

	// NOTE: The others can be nullptr;
}

void BottomDoorlet::draw(CanvasDrawingSession^ ds, float x, float y, float Width, float Height) {
	const DoorStyle style = this->get_style();
	float cx = x + this->radius + default_thickness;
	float cy = y + this->radius + default_thickness;
	float body_radius = this->radius - default_thickness * 1.618F;
	
	ds->FillCircle(cx, cy, this->radius, Colours::Background);
	ds->FillCircle(cx, cy, body_radius, style.body_color);
	
	for (unsigned int idx = 0; idx < sizeof(this->door_partitions) / sizeof(CanvasGeometry^); idx++) {
		ds->FillGeometry(this->door_partitions[idx], cx, cy, style.door_color);
		ds->DrawGeometry(this->door_partitions[idx], cx, cy, style.skeleton_color);
	}

	if (style.disable_color != nullptr) {
		if (this->disable_line != nullptr) { // this is always true
			ds->DrawCachedGeometry(this->disable_line, cx, cy, style.disable_color);
		}
	}

	if (this->flashing) {
		ds->DrawCircle(cx, cy, this->radius, style.border_hlcolor, default_thickness);
	} else {
		ds->DrawCircle(cx, cy, this->radius, style.border_color, default_thickness);
	}
}

void BottomDoorlet::make_masked_door_partitions() {
	double ratio = this->mask_percentage;

	this->door_partitions[0] = masked_sector(this->degrees + 60.00, this->degrees - 60.00, ratio, this->radius);
	this->door_partitions[1] = masked_sector(this->degrees - 60.00, this->degrees - 180.0, ratio, this->radius);
	this->door_partitions[2] = masked_sector(this->degrees + 180.0, this->degrees + 60.00, ratio, this->radius);
}

/*************************************************************************************************/
UpperDoorlet::UpperDoorlet(float radius, double degrees)
	: UpperDoorlet(DoorStatus::Closed, radius, degrees) {}

UpperDoorlet::UpperDoorlet(DoorStatus default_state, float radius, double degrees)
	: ISymbollet(default_state, radius, degrees) {
	this->radius = radius - default_thickness;
	this->bradius = this->radius - default_thickness * 1.618F;
}

void UpperDoorlet::construct() {
	auto pline = polar_line(this->radius, this->degrees, this->degrees + 180.0);
	
	this->border = polar_rectangle(this->radius, default_alpha_degrees, this->degrees);
	this->disable_line = geometry_draft(geometry_intersect(this->border, pline), default_thickness);
	this->body = geometry_freeze(polar_rectangle(this->bradius, default_alpha_degrees, this->degrees));
}

void UpperDoorlet::update(long long count, long long interval, long long uptime) {
	switch (this->get_status()) {
	case DoorStatus::Opening: {
		this->mask_percentage
			= ((this->mask_percentage <= 0.0) || (this->mask_percentage > 1.0))
			? 1.0
			: this->mask_percentage - dynamic_mask_interval;
		
		this->flashing = !this->flashing;
		this->make_masked_door();
		this->notify_updated();
	} break;
	case DoorStatus::Closing: {
		this->mask_percentage
			= ((this->mask_percentage < 0.0) || (this->mask_percentage >= 1.0))
			? 0.0
			: this->mask_percentage + dynamic_mask_interval;

		this->flashing = !this->flashing;
		this->make_masked_door();
		this->notify_updated();
	} break;
	}
}

void UpperDoorlet::on_status_changed(DoorStatus state) {
	this->flashing = false;

	switch (state) {
	case DoorStatus::Open: {
		this->mask_percentage = 0.0;
		this->make_masked_door();
		this->notify_updated();
	} break;
	case DoorStatus::Closed: case DoorStatus::Disabled: {
		this->mask_percentage = 1.0;
		this->make_masked_door();
		this->notify_updated();
	} break;
	}
}

void UpperDoorlet::prepare_style(DoorStatus state, DoorStyle& s) {
	switch (state) {
	case DoorStatus::Disabled: {
		CAS_SLOT(s.disable_color, Colours::Firebrick);
	} break;
	case DoorStatus::Closed: {
		CAS_SLOT(s.door_color, Colours::Gray);
	} break;
	case DoorStatus::Opening: {
		CAS_SLOT(s.border_hlcolor, Colours::Green);
	}; break;
	case DoorStatus::Closing: {
		CAS_SLOT(s.border_hlcolor, Colours::Yellow);
	}; break;
	}

	CAS_SLOT(s.border_color, Colours::ForestGreen);
	CAS_SLOT(s.border_hlcolor, s.border_color);
	CAS_SLOT(s.door_color, Colours::DimGray);
	CAS_SLOT(s.body_color, Colours::DarkKhaki);
	CAS_SLOT(s.skeleton_color, Colours::Black);

	// NOTE: The others can be nullptr;
}

void UpperDoorlet::draw(CanvasDrawingSession^ ds, float x, float y, float Width, float Height) {
	const DoorStyle style = this->get_style();
	float cx = x + this->radius + default_thickness;
	float cy = y + this->radius + default_thickness;

	ds->FillGeometry(this->border, cx, cy, Colours::Background);
	ds->DrawCachedGeometry(this->body, cx, cy, style.body_color);

	ds->FillGeometry(this->door, cx, cy, style.door_color);
	ds->DrawGeometry(this->door, cx, cy, style.skeleton_color);
	
	if (style.disable_color != nullptr) {
		if (this->disable_line != nullptr) { // this is always true
			ds->DrawCachedGeometry(this->disable_line, cx, cy, style.disable_color);
		}
	}

	if (this->flashing) {
		ds->DrawGeometry(this->border, cx, cy, style.border_hlcolor, default_thickness);
	} else {
		ds->DrawGeometry(this->border, cx, cy, style.border_color, default_thickness);
	}
}

void UpperDoorlet::make_masked_door() {
	this->door = polar_masked_rectangle(this->bradius, default_alpha_degrees, this->degrees, -this->mask_percentage);
}
