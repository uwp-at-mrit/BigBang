#include "graphlet/symbol/door/hopper_doorlet.hpp"

#include "polar.hpp"
#include "shape.hpp"
#include "paint.hpp"
#include "brushes.hxx"
#include "geometry.hpp"

using namespace WarGrey::SCADA;

using namespace Windows::Foundation;

using namespace Microsoft::Graphics::Canvas;
using namespace Microsoft::Graphics::Canvas::Brushes;
using namespace Microsoft::Graphics::Canvas::Geometry;

static float default_thickness = 2.0F;
static double default_alpha_degrees = 45.0;

static CanvasSolidColorBrush^ door_default_border_color = Colours::make(0xBBBBBB);
static CanvasSolidColorBrush^ door_default_bottom_color = Colours::make(0xBB6666);
static CanvasSolidColorBrush^ door_default_progress_color = Colours::Yellow;

/*************************************************************************************************/
HopperDoorlet::HopperDoorlet(float radius, double degrees) : HopperDoorlet(DoorStatus::Closed, radius, degrees) {}

HopperDoorlet::HopperDoorlet(DoorStatus default_state, float radius, double degrees)
	: ISymbollet(default_state, radius, degrees), IRangelet(0.0, 1.0) {}

void HopperDoorlet::update(long long count, long long interval, long long uptime) {
	if (!this->stopped) {
		switch (this->get_status()) {
		case DoorStatus::Opening: case DoorStatus::Closing: {
			this->flashing = !this->flashing;
			this->notify_updated();
		}; break;
		}
	}
}

void HopperDoorlet::prepare_style(DoorStatus state, DoorStyle& s) {
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
	CAS_SLOT(s.body_color, door_default_progress_color);
	CAS_SLOT(s.skeleton_color, Colours::Black);

	// NOTE: The others can be nullptr;
}

void HopperDoorlet::on_status_changed(DoorStatus state) {
	this->flashing = false;
	this->stopped = false;

	switch (state) {
	case DoorStatus::Open: {
		this->set_value(1.0, true);
	} break;
	case DoorStatus::Disabled: {
		if (this->disable_line == nullptr) {
			double d0 = this->degrees - 45.0;
			double dn = this->degrees + 135.0;

			this->disable_line = geometry_draft(polar_line(this->radiusX - default_thickness, d0, dn), default_thickness);
		}
	} // NOTE: there is no `break` here;
	case DoorStatus::Closed: {
		this->set_value(0.0, true);
	} break;
	}
}

void HopperDoorlet::stop() {
	this->stopped = true;
	this->flashing = false;
}

void HopperDoorlet::on_value_changed(double v) {
	double ratio = this->get_percentage();
	float r = this->radiusX - default_thickness;

	this->door_partitions[0] = masked_sector(this->degrees + 60.00, this->degrees - 60.00, ratio, r);
	this->door_partitions[1] = masked_sector(this->degrees - 60.00, this->degrees - 180.0, ratio, r);
	this->door_partitions[2] = masked_sector(this->degrees + 180.0, this->degrees + 60.00, ratio, r);
}

void HopperDoorlet::draw(CanvasDrawingSession^ ds, float x, float y, float Width, float Height) {
	const DoorStyle style = this->get_style();
	float border_radius = this->radiusX - default_thickness;
	float body_radius = border_radius - default_thickness * 1.618F;
	float cx = x + this->radiusX;
	float cy = y + this->radiusY;
	
	ds->FillCircle(cx, cy, border_radius, Colours::Background);
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
		ds->DrawCircle(cx, cy, border_radius, style.border_hlcolor, default_thickness);
	} else {
		ds->DrawCircle(cx, cy, border_radius, style.border_color, default_thickness);
	}
}

/*************************************************************************************************/
UpperHopperDoorlet::UpperHopperDoorlet(float radius, double degrees)
	: UpperHopperDoorlet(DoorStatus::Closed, radius, degrees) {}

UpperHopperDoorlet::UpperHopperDoorlet(DoorStatus default_state, float radius, double degrees)
	: ISymbollet(default_state, radius, degrees), IRangelet(0.0, 1.0) {
	this->radiusY = this->radiusX * 0.618F;
	this->brdiff = default_thickness * 1.618F;
}

void UpperHopperDoorlet::construct() {
	auto pline = geometry_rotate(polar_line(this->radiusX, this->radiusY, 0.0, 180.0), this->degrees);
	
	this->border = geometry_rotate(polar_rectangle(this->radiusX, this->radiusY, default_alpha_degrees, 0.0), this->degrees);
	this->disable_line = geometry_draft(geometry_intersect(this->border, pline), default_thickness);
	
	this->body = geometry_freeze(geometry_rotate(polar_rectangle(
		this->radiusX - this->brdiff, this->radiusY - this->brdiff,
		default_alpha_degrees, 0.0), this->degrees));
}

void UpperHopperDoorlet::fill_margin(float x, float y, float* top, float* right, float* bottom, float* left) {
	auto box = this->border->ComputeStrokeBounds(default_thickness);
	float hspace = this->width - box.Width;
	float vspace = this->height - box.Height;

	SET_BOXES(top, bottom, vspace * 0.5F);
	SET_BOXES(left, right, hspace * 0.5F);
}

void UpperHopperDoorlet::update(long long count, long long interval, long long uptime) {
	if (!this->stopped) {
		switch (this->get_status()) {
		case DoorStatus::Opening: case DoorStatus::Closing: {
			this->flashing = !this->flashing;
			this->notify_updated();
		}; break;
		}
	}
}

void UpperHopperDoorlet::prepare_style(DoorStatus state, DoorStyle& s) {
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

	CAS_SLOT(s.border_color, Colours::Teal);
	CAS_SLOT(s.border_hlcolor, s.border_color);
	CAS_SLOT(s.door_color, Colours::DimGray);
	CAS_SLOT(s.body_color, Colours::DarkKhaki);
	CAS_SLOT(s.skeleton_color, Colours::Black);

	// NOTE: The others can be nullptr;
}

void UpperHopperDoorlet::stop() {
	this->stopped = true;
	this->flashing = false;
}

void UpperHopperDoorlet::on_status_changed(DoorStatus state) {
	this->flashing = false;
	this->stopped = false;

	switch (state) {
	case DoorStatus::Open: {
		this->set_value(1.0, true);
	} break;
	case DoorStatus::Closed: case DoorStatus::Disabled: {
		this->set_value(0.0, true);
	} break;
	}
}

void UpperHopperDoorlet::on_value_changed(double v) {
	float rx = this->radiusX - this->brdiff;
	float ry = this->radiusY - this->brdiff;
	auto partial_door = polar_masked_rectangle(rx, ry, default_alpha_degrees, 0.0F, this->get_percentage() - 1.0);

	/** Note: all masked doors must be rotated with (0.0F, 0.0F) */
	this->door = geometry_rotate(partial_door, this->degrees, 0.0F, 0.0F);
}

void UpperHopperDoorlet::draw(CanvasDrawingSession^ ds, float x, float y, float Width, float Height) {
	const DoorStyle style = this->get_style();
	float cx = x + this->width * 0.5F;
	float cy = y + this->height * 0.5F;

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

/*************************************************************************************************/
Doorlet::Doorlet(float width, float height, float thickness, ICanvasBrush^ color
	, CanvasSolidColorBrush^ bdcolor, CanvasSolidColorBrush^ btmcolor)
	: IRangelet(0.0, 1.0), width(width), height(height), thickness(thickness)
	, color((color == nullptr) ? door_default_progress_color : color)
	, border_color((bdcolor == nullptr) ? door_default_border_color : bdcolor)
	, bottom_color((btmcolor == nullptr) ? door_default_bottom_color : btmcolor) {
	if (this->height == 0.0F) {
		this->height = this->width * 6.18F;
	}
}

void Doorlet::construct() {
	float radius = this->thickness;
	float thickoff = this->thickness * 0.5F;
	float base_width = this->width * 0.1618F;
	float bottom_x = base_width + this->thickness;
	float bottom_y = this->height * 0.5F;
	float bottom_width = this->width - bottom_x * 2.0F;
	float bottom_height = this->height - bottom_y - thickoff;
	float top_width = bottom_width * 0.5F;
	float top_x = bottom_x + (bottom_width - top_width) * 0.5F;
	float top_y = thickoff;
	float top_height = bottom_y + this->thickness;
	float base_height = bottom_height * 0.382F;
	float base_bottom = this->height;
	float base_left = 0.0F;
	float base_right = this->width;
	float base_y = base_bottom - base_height;
	auto top = rounded_rectangle(top_x, top_y, top_width, top_height, radius, radius);
	auto bottom = rectangle(bottom_x, bottom_y, bottom_width, bottom_height);
	auto lbase = triangle(base_left + base_width, base_y, base_left, base_bottom);
	auto rbase = triangle(base_right - base_width, base_y, base_right, base_bottom);
	
	this->top = geometry_draft(geometry_subtract(top, bottom), this->thickness * 1.618F);
	this->bottom = geometry_draft(geometry_subtract(bottom, top), this->thickness * 1.618F);
	this->base = geometry_freeze(geometry_union(lbase, rbase));
	this->body = geometry_union(top, bottom);

	this->set_value(0.0, true);
}

void Doorlet::fill_extent(float x, float y, float* w, float* h) {
	SET_VALUES(w, this->width, h, this->height);
}

void Doorlet::on_value_changed(double v) {
	Rect region = this->body->ComputeBounds();
	float hollow_height = region.Height * float(this->get_percentage());
	auto pbox = rectangle(region.X, region.Y, region.Width, hollow_height);

	this->progress = geometry_freeze(geometry_subtract(this->body, pbox));
}

void Doorlet::draw(CanvasDrawingSession^ ds, float x, float y, float Width, float Height) {
	ds->DrawCachedGeometry(this->top, x, y, this->border_color);
	ds->DrawCachedGeometry(this->bottom, x, y, this->bottom_color);
	ds->DrawCachedGeometry(this->base, x, y, this->bottom_color);
	
	ds->FillGeometry(this->body, x, y, Colours::Background);
	ds->DrawCachedGeometry(this->progress, x, y, this->color);
}
