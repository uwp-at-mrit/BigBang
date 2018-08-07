#include "graphlet/symbol/doorlet.hpp"

#include "math.hpp"
#include "shape.hpp"
#include "brushes.hxx"

using namespace WarGrey::SCADA;

using namespace Microsoft::Graphics::Canvas;
using namespace Microsoft::Graphics::Canvas::Brushes;
using namespace Microsoft::Graphics::Canvas::Geometry;

static float default_thickness = 2.0F;
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

		this->make_masked_door_partitions();
		this->notify_updated();
	} break;
	case DoorStatus::Closing: {
		this->mask_percentage
			= ((this->mask_percentage <= 0.0) || (this->mask_percentage > 1.0))
			? 1.0
			: this->mask_percentage - dynamic_mask_interval;

		this->make_masked_door_partitions();
		this->notify_updated();
	} break;
	}
}

void BottomDoorlet::on_status_changed(DoorStatus state) {
	switch (state) {
	case DoorStatus::Open: {
		this->mask_percentage = 1.0;
		this->make_masked_door_partitions();
		this->notify_updated();
	} break;
	case DoorStatus::Disabled: {
		if (this->disable_line == nullptr) {
			float x1, y1, x2, y2;

			circle_point(this->radius, this->degrees - 45.0, &x1, &y1);
			circle_point(this->radius, this->degrees + 135.0, &x2, &y2);

			this->disable_line = geometry_freeze(line(x1, y1, x2, y2, default_thickness));
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
	}

	CAS_SLOT(s.border_color, Colours::ForestGreen);
	CAS_SLOT(s.door_color, Colours::DimGray);
	CAS_SLOT(s.body_color, Colours::Khaki);
	CAS_SLOT(s.skeleton_color, Colours::Black);

	// NOTE: The others can be nullptr;
}

void BottomDoorlet::draw(CanvasDrawingSession^ ds, float x, float y, float Width, float Height) {
	const DoorStyle style = this->get_style();
	float cx = x + this->radius + default_thickness;
	float cy = y + this->radius + default_thickness;
	
	ds->FillCircle(cx, cy, this->radius, Colours::Background);
	ds->FillCircle(cx, cy, this->radius - default_thickness * 2.0F, style.body_color);
	
	for (unsigned int idx = 0; idx < sizeof(this->door_partitions) / sizeof(CanvasGeometry^); idx++) {
		ds->FillGeometry(this->door_partitions[idx], cx, cy, style.door_color);
		ds->DrawGeometry(this->door_partitions[idx], cx, cy, style.skeleton_color);
	}

	if (style.disable_color != nullptr) {
		if (this->disable_line != nullptr) { // this is always true
			ds->DrawCachedGeometry(this->disable_line, cx, cy, style.disable_color);
		}
	}

	ds->DrawCircle(cx, cy, this->radius, style.border_color, default_thickness);
}

void BottomDoorlet::make_masked_door_partitions() {
	double ratio = this->mask_percentage;

	this->door_partitions[0] = masked_sector(this->degrees + 60.00, this->degrees - 60.00, ratio, this->radius);
	this->door_partitions[1] = masked_sector(this->degrees - 60.00, this->degrees - 180.0, ratio, this->radius);
	this->door_partitions[2] = masked_sector(this->degrees + 180.0, this->degrees + 60.00, ratio, this->radius);
}
