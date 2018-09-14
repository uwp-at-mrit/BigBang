#include "graphlet/symbol/valve/gate_valvelet.hpp"

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

/*************************************************************************************************/
GateValveStyle WarGrey::SCADA::make_manual_valve_style(ICanvasBrush^ color) {
	GateValveStyle s;

	s.stem_color = ((color == nullptr) ? Colours::Gray : color);

	return s;
}

/*************************************************************************************************/
GateValvelet::GateValvelet(float radius, double degrees) : GateValvelet(GateValveStatus::Disabled, radius, degrees) {}

GateValvelet::GateValvelet(GateValveStatus default_status, float radius, double degrees)
	: ISymbollet(default_status, radius, degrees) {
	this->fradius = radius;
	this->sgradius = this->fradius - default_thickness * 2.0F;
}

void GateValvelet::construct() {
	double adjust_degrees = this->degrees + 90.0;
	float stem_length = this->sgradius * 0.618F;
	auto stem = polar_axis(stem_length, this->degrees - 90.0);
	auto wheel = polar_pole(stem_length, this->degrees - 90.0, stem_length * 0.1618F);

	this->frame = polar_rectangle(this->fradius, 60.0, adjust_degrees);
	this->shaft = geometry_union(stem, wheel);
	this->skeleton = polar_sandglass(this->sgradius, adjust_degrees);
	this->body = geometry_freeze(this->skeleton);
}

void GateValvelet::update(long long count, long long interval, long long uptime) {
	double adjust_degrees = this->degrees + 90.0;

	switch (this->get_status()) {
	case GateValveStatus::Opening: {
		this->mask_percentage
			= ((this->mask_percentage < 0.0) || (this->mask_percentage >= 1.0))
			? 0.0
			: this->mask_percentage + dynamic_mask_interval;

		this->mask = polar_masked_sandglass(this->sgradius, adjust_degrees, -this->mask_percentage);
		this->notify_updated();
	} break;
	case GateValveStatus::Closing: {
		this->mask_percentage
			= ((this->mask_percentage <= 0.0) || (this->mask_percentage > 1.0))
			? 1.0
			: this->mask_percentage - dynamic_mask_interval;

		this->mask = polar_masked_sandglass(this->sgradius, adjust_degrees, this->mask_percentage);
		this->notify_updated();
	} break;
	}
}

void GateValvelet::prepare_style(GateValveStatus status, GateValveStyle& s) {
	switch (status) {
	case GateValveStatus::Disabled: {
		CAS_SLOT(s.mask_color, Colours::Teal);
	}; break;
	case GateValveStatus::Open: {
		CAS_SLOT(s.body_color, Colours::Green);
	}; break;
	case GateValveStatus::Opening: {
		CAS_SLOT(s.mask_color, Colours::Green);
	}; break;
	case GateValveStatus::OpenReady: {
		CAS_VALUES(s.skeleton_color, Colours::Cyan, s.mask_color, Colours::ForestGreen);
	}; break;
	case GateValveStatus::Unopenable: {
		CAS_VALUES(s.skeleton_color, Colours::Red, s.mask_color, Colours::Green);
	}; break;
	case GateValveStatus::Closed: {
		CAS_SLOT(s.body_color, Colours::Gray);
	}; break;
	case GateValveStatus::Closing: {
		CAS_SLOT(s.mask_color, Colours::DarkGray);
	}; break;
	case GateValveStatus::CloseReady: {
		CAS_VALUES(s.skeleton_color, Colours::Cyan, s.mask_color, Colours::DimGray);
	}; break;
	case GateValveStatus::Unclosable: {
		CAS_VALUES(s.skeleton_color, Colours::Red, s.mask_color, Colours::DarkGray);
	}; break;
	case GateValveStatus::FakeOpen: {
		CAS_VALUES(s.frame_color, Colours::Red, s.body_color, Colours::ForestGreen);
	}; break;
	case GateValveStatus::FakeClose: {
		CAS_VALUES(s.frame_color, Colours::Red, s.body_color, Colours::DimGray);
	}; break;
	}

	CAS_SLOT(s.skeleton_color, Colours::DarkGray);
	CAS_SLOT(s.body_color, Colours::Background);
	CAS_SLOT(s.stem_color, Colours::Background);

	// NOTE: The others can be nullptr;
}

void GateValvelet::on_status_changed(GateValveStatus status) {
	double adjust_degrees = this->degrees + 90.0;

	switch (status) {
	case GateValveStatus::Unopenable: {
		if (this->bottom_up_mask == nullptr) {
			this->bottom_up_mask = polar_masked_sandglass(this->sgradius, adjust_degrees, -0.80);
		}
		this->mask = this->bottom_up_mask;
	} break;
	case GateValveStatus::Unclosable: case GateValveStatus::Disabled: {
		if (this->top_down_mask == nullptr) {
			this->top_down_mask = polar_masked_sandglass(this->sgradius, adjust_degrees, 0.80);
		}
		this->mask = this->top_down_mask;
	} break;
	case GateValveStatus::OpenReady: {
		if (this->bottom_up_ready_mask == nullptr) {
			this->bottom_up_ready_mask = polar_masked_sandglass(this->sgradius, adjust_degrees, -0.70);
		}
		this->mask = this->bottom_up_ready_mask;
	} break;
	case GateValveStatus::CloseReady: {
		if (this->top_down_ready_mask == nullptr) {
			this->top_down_ready_mask = polar_masked_sandglass(this->sgradius, adjust_degrees, 0.70);
		}
		this->mask = this->top_down_ready_mask;
	} break;
	default: {
		this->mask = nullptr;
		this->mask_percentage = -1.0;
	}
	}
}

void GateValvelet::draw(CanvasDrawingSession^ ds, float x, float y, float Width, float Height) {
	const GateValveStyle style = this->get_style();
	float radiusX = this->width * 0.5F - default_thickness;
	float radiusY = this->width * 0.5F - default_thickness;
	float cx = x + radiusX + default_thickness;
	float cy = y + radiusY + default_thickness;
	
	if (style.stem_color != Colours::Background) {
		ds->DrawGeometry(this->shaft, cx, cy, style.stem_color, default_thickness);
	}
	
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
