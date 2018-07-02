#include "graphlet/symbol/valvelet.hpp"

#include "polar.hpp"
#include "paint.hpp"
#include "geometry.hpp"
#include "brushes.hxx"

using namespace WarGrey::SCADA;

using namespace Microsoft::Graphics::Canvas;
using namespace Microsoft::Graphics::Canvas::Brushes;

static float default_thickness = 1.5F;
static double dynamic_mask_interval = 1.0 / 8.0;

static CanvasSolidColorBrush^ default_sketeton_color = Colours::DarkGray;

/*************************************************************************************************/
Valvelet::Valvelet(float radius, double degrees) : Valvelet(ValveStatus::Manual, radius, degrees) {}

Valvelet::Valvelet(ValveStatus default_status, float radius, double degrees) : ISymbollet(default_status, radius, degrees) {
	this->fradius = radius;
	this->sgradius = this->fradius - default_thickness * 2.0F;
	this->update_status();
}

void Valvelet::construct() {
	float handle_length = this->sgradius * 0.618F;
	auto handler_axis = polar_axis(handle_length, this->degrees);
	auto handler_pole = polar_pole(handle_length, this->degrees, handle_length * 0.1618F);

	this->frame = polar_rectangle(this->fradius, 60.0, this->degrees);
	this->handle = geometry_union(handler_axis, handler_pole);
	this->skeleton = polar_sandglass(this->sgradius, this->degrees);
	this->body = geometry_freeze(this->skeleton);
}

void Valvelet::update(long long count, long long interval, long long uptime) {
	switch (this->get_status()) {
	case ValveStatus::Opening: {
		this->mask_percentage
			= ((this->mask_percentage < 0.0) || (this->mask_percentage >= 1.0))
			? 0.0
			: this->mask_percentage + dynamic_mask_interval;

		this->mask = polar_masked_sandglass(this->sgradius, this->degrees, this->mask_percentage);
		this->notify_updated();
	} break;
	case ValveStatus::Closing: {
		this->mask_percentage
			= ((this->mask_percentage <= 0.0) || (this->mask_percentage > 1.0))
			? 1.0
			: this->mask_percentage - dynamic_mask_interval;

		this->mask = polar_masked_sandglass(this->sgradius, this->degrees, -this->mask_percentage);
		this->notify_updated();
	} break;
	}
}

void Valvelet::prepare_style(ValveStatus status, ValveStyle& s) {
	switch (status) {
	case ValveStatus::Manual: CAS_SLOT(s.mask_color, Colours::Teal); break;
	case ValveStatus::Open: CAS_SLOT(s.body_color, Colours::Green); break;
	case ValveStatus::Opening: CAS_SLOT(s.mask_color, Colours::Green); break;
	case ValveStatus::OpenReady: CAS_VALUES(s.skeleton_color, Colours::Cyan, s.mask_color, Colours::ForestGreen); break;
	case ValveStatus::Unopenable: CAS_VALUES(s.skeleton_color, Colours::Red, s.mask_color, Colours::Green); break;
	case ValveStatus::Closed: CAS_SLOT(s.body_color, Colours::LightGray); break;
	case ValveStatus::Closing: CAS_SLOT(s.mask_color, Colours::DarkGray); break;
	case ValveStatus::CloseReady: CAS_VALUES(s.skeleton_color, Colours::Cyan, s.mask_color, Colours::DimGray); break;
	case ValveStatus::Unclosable: CAS_VALUES(s.skeleton_color, Colours::Red, s.mask_color, Colours::DarkGray); break;
	case ValveStatus::FalseOpen: CAS_VALUES(s.border_color, Colours::Red, s.body_color, Colours::ForestGreen); break;
	case ValveStatus::FalseClosed: CAS_VALUES(s.border_color, Colours::Red, s.body_color, Colours::DimGray); break;
	}

	CAS_SLOT(s.skeleton_color, default_sketeton_color);
	CAS_SLOT(s.body_color, Colours::Background);

	// NOTE: The others can be nullptr;
}

void Valvelet::on_status_changed(ValveStatus status) {
	switch (status) {
	case ValveStatus::Unopenable: {
		if (this->bottom_up_mask == nullptr) {
			this->bottom_up_mask = polar_masked_sandglass(this->sgradius, this->degrees, 0.80);
		}
		this->mask = this->bottom_up_mask;
	} break;
	case ValveStatus::Unclosable: case ValveStatus::Manual: {
		if (this->top_down_mask == nullptr) {
			this->top_down_mask = polar_masked_sandglass(this->sgradius, this->degrees, -0.80);
		}
		this->mask = this->top_down_mask;
	} break;
	case ValveStatus::OpenReady: {
		if (this->bottom_up_ready_mask == nullptr) {
			this->bottom_up_ready_mask = polar_masked_sandglass(this->sgradius, this->degrees, 0.70);
		}
		this->mask = this->bottom_up_ready_mask;
	} break;
	case ValveStatus::CloseReady: {
		if (this->top_down_ready_mask == nullptr) {
			this->top_down_ready_mask = polar_masked_sandglass(this->sgradius, this->degrees, -0.70);
		}
		this->mask = this->top_down_ready_mask;
	} break;
	default: {
		this->mask = nullptr;
		this->mask_percentage = -1.0;
	}
	}
}

void Valvelet::draw(CanvasDrawingSession^ ds, float x, float y, float Width, float Height) {
	const ValveStyle style = this->get_style();
	
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
