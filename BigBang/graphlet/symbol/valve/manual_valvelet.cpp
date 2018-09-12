#include "graphlet/symbol/valve/manual_valvelet.hpp"

#include "string.hpp"

#include "math.hpp"
#include "text.hpp"
#include "shape.hpp"
#include "polar.hpp"
#include "paint.hpp"
#include "geometry.hpp"
#include "brushes.hxx"

using namespace WarGrey::SCADA;

using namespace Windows::Foundation;

using namespace Microsoft::Graphics::Canvas;
using namespace Microsoft::Graphics::Canvas::Brushes;

static float default_thickness = 1.5F;
static double dynamic_mask_interval = 1.0 / 8.0;

ManualValveStyle WarGrey::SCADA::make_handle_valve_style(ICanvasBrush^ color) {
	ManualValveStyle s;

	s.handle_color = ((color == nullptr) ? Colours::Gray : color);

	return s;
}

/*************************************************************************************************/
ManualValvelet::ManualValvelet(float radius, double degrees) : ManualValvelet(ManualValveStatus::Disabled, radius, degrees) {}

ManualValvelet::ManualValvelet(ManualValveStatus default_status, float radius, double degrees)
	: ISymbollet(default_status, radius, degrees) {
	this->fradius = radius;
	this->sgradius = this->fradius - default_thickness * 2.0F;
}

void ManualValvelet::construct() {
	float handle_length = this->sgradius * 0.618F;
	auto handle_axis = polar_axis(handle_length, this->degrees);
	auto handle_pole = polar_pole(handle_length, this->degrees, handle_length * 0.1618F);

	this->frame = polar_rectangle(this->fradius, 60.0, this->degrees);
	this->handle = geometry_union(handle_axis, handle_pole);
	this->skeleton = polar_sandglass(this->sgradius, this->degrees);
	this->body = geometry_freeze(this->skeleton);
}

void ManualValvelet::update(long long count, long long interval, long long uptime) {
	switch (this->get_status()) {
	case ManualValveStatus::Opening: {
		this->mask_percentage
			= ((this->mask_percentage < 0.0) || (this->mask_percentage >= 1.0))
			? 0.0
			: this->mask_percentage + dynamic_mask_interval;

		this->mask = polar_masked_sandglass(this->sgradius, this->degrees, this->mask_percentage);
		this->notify_updated();
	} break;
	case ManualValveStatus::Closing: {
		this->mask_percentage
			= ((this->mask_percentage <= 0.0) || (this->mask_percentage > 1.0))
			? 1.0
			: this->mask_percentage - dynamic_mask_interval;

		this->mask = polar_masked_sandglass(this->sgradius, this->degrees, -this->mask_percentage);
		this->notify_updated();
	} break;
	}
}

void ManualValvelet::prepare_style(ManualValveStatus status, ManualValveStyle& s) {
	switch (status) {
	case ManualValveStatus::Disabled: {
		CAS_SLOT(s.mask_color, Colours::Teal);
	}; break;
	case ManualValveStatus::Open: {
		CAS_SLOT(s.body_color, Colours::Green);
	}; break;
	case ManualValveStatus::Opening: {
		CAS_SLOT(s.mask_color, Colours::Green);
	}; break;
	case ManualValveStatus::OpenReady: {
		CAS_VALUES(s.skeleton_color, Colours::Cyan, s.mask_color, Colours::ForestGreen);
	}; break;
	case ManualValveStatus::Unopenable: {
		CAS_VALUES(s.skeleton_color, Colours::Red, s.mask_color, Colours::Green);
	}; break;
	case ManualValveStatus::Closed: {
		CAS_SLOT(s.body_color, Colours::Gray);
	}; break;
	case ManualValveStatus::Closing: {
		CAS_SLOT(s.mask_color, Colours::DarkGray);
	}; break;
	case ManualValveStatus::CloseReady: {
		CAS_VALUES(s.skeleton_color, Colours::Cyan, s.mask_color, Colours::DimGray);
	}; break;
	case ManualValveStatus::Unclosable: {
		CAS_VALUES(s.skeleton_color, Colours::Red, s.mask_color, Colours::DarkGray);
	}; break;
	case ManualValveStatus::FakeOpen: {
		CAS_VALUES(s.frame_color, Colours::Red, s.body_color, Colours::ForestGreen);
	}; break;
	case ManualValveStatus::FakeClose: {
		CAS_VALUES(s.frame_color, Colours::Red, s.body_color, Colours::DimGray);
	}; break;
	}

	CAS_SLOT(s.skeleton_color, Colours::DarkGray);
	CAS_SLOT(s.body_color, Colours::Background);
	CAS_SLOT(s.handle_color, Colours::Background);

	// NOTE: The others can be nullptr;
}

void ManualValvelet::on_status_changed(ManualValveStatus status) {
	switch (status) {
	case ManualValveStatus::Unopenable: {
		if (this->bottom_up_mask == nullptr) {
			this->bottom_up_mask = polar_masked_sandglass(this->sgradius, this->degrees, 0.80);
		}
		this->mask = this->bottom_up_mask;
	} break;
	case ManualValveStatus::Unclosable: case ManualValveStatus::Disabled: {
		if (this->top_down_mask == nullptr) {
			this->top_down_mask = polar_masked_sandglass(this->sgradius, this->degrees, -0.80);
		}
		this->mask = this->top_down_mask;
	} break;
	case ManualValveStatus::OpenReady: {
		if (this->bottom_up_ready_mask == nullptr) {
			this->bottom_up_ready_mask = polar_masked_sandglass(this->sgradius, this->degrees, 0.70);
		}
		this->mask = this->bottom_up_ready_mask;
	} break;
	case ManualValveStatus::CloseReady: {
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

void ManualValvelet::draw(CanvasDrawingSession^ ds, float x, float y, float Width, float Height) {
	const ManualValveStyle style = this->get_style();
	float radius = this->size * 0.5F - default_thickness;
	float cx = x + radius + default_thickness;
	float cy = y + radius + default_thickness;
	
	ds->DrawGeometry(this->handle, cx, cy, style.handle_color, default_thickness);
	
	if (style.frame_color != nullptr) {
		ds->DrawGeometry(this->frame, cx, cy, style.frame_color, default_thickness);
	}

	ds->DrawCachedGeometry(this->body, cx, cy, style.body_color);

	if (style.mask_color != nullptr) {
		auto mask = ((this->mask == nullptr) ? this->skeleton : this->mask);
		
		ds->FillGeometry(mask, cx, cy, style.mask_color);
		ds->DrawGeometry(mask, cx, cy, style.mask_color, default_thickness);
	}

	ds->DrawGeometry(this->skeleton, cx, cy, style.skeleton_color, default_thickness);
}
