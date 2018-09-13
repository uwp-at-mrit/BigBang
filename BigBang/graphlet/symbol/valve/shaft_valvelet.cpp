#include "graphlet/symbol/valve/shaft_valvelet.hpp"

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
ShaftValveStyle WarGrey::SCADA::make_shaft_valve_style(ICanvasBrush^ color) {
	ShaftValveStyle s;

	s.shaft_color = ((color == nullptr) ? Colours::Gray : color);

	return s;
}

/*************************************************************************************************/
ShaftValvelet::ShaftValvelet(float radius, double degrees) : ShaftValvelet(ShaftValveStatus::Disabled, radius, degrees) {}

ShaftValvelet::ShaftValvelet(ShaftValveStatus default_status, float radius, double degrees)
	: ISymbollet(default_status, radius, degrees) {
	this->fradius = radius;
	this->sgradius = this->fradius - default_thickness * 2.0F;
}

void ShaftValvelet::construct() {
	double adjust_degrees = this->degrees + 90.0;
	float shaft_length = this->sgradius * 0.618F;
	auto shaft_axis = polar_axis(shaft_length, this->degrees - 90.0);
	auto shaft_pole = polar_pole(shaft_length, this->degrees - 90.0, shaft_length * 0.1618F);

	this->frame = polar_rectangle(this->fradius, 60.0, adjust_degrees);
	this->shaft = geometry_union(shaft_axis, shaft_pole);
	this->skeleton = polar_sandglass(this->sgradius, adjust_degrees);
	this->body = geometry_freeze(this->skeleton);
}

void ShaftValvelet::update(long long count, long long interval, long long uptime) {
	double adjust_degrees = this->degrees + 90.0;

	switch (this->get_status()) {
	case ShaftValveStatus::Opening: {
		this->mask_percentage
			= ((this->mask_percentage < 0.0) || (this->mask_percentage >= 1.0))
			? 0.0
			: this->mask_percentage + dynamic_mask_interval;

		this->mask = polar_masked_sandglass(this->sgradius, adjust_degrees, -this->mask_percentage);
		this->notify_updated();
	} break;
	case ShaftValveStatus::Closing: {
		this->mask_percentage
			= ((this->mask_percentage <= 0.0) || (this->mask_percentage > 1.0))
			? 1.0
			: this->mask_percentage - dynamic_mask_interval;

		this->mask = polar_masked_sandglass(this->sgradius, adjust_degrees, this->mask_percentage);
		this->notify_updated();
	} break;
	}
}

void ShaftValvelet::prepare_style(ShaftValveStatus status, ShaftValveStyle& s) {
	switch (status) {
	case ShaftValveStatus::Disabled: {
		CAS_SLOT(s.mask_color, Colours::Teal);
	}; break;
	case ShaftValveStatus::Open: {
		CAS_SLOT(s.body_color, Colours::Green);
	}; break;
	case ShaftValveStatus::Opening: {
		CAS_SLOT(s.mask_color, Colours::Green);
	}; break;
	case ShaftValveStatus::OpenReady: {
		CAS_VALUES(s.skeleton_color, Colours::Cyan, s.mask_color, Colours::ForestGreen);
	}; break;
	case ShaftValveStatus::Unopenable: {
		CAS_VALUES(s.skeleton_color, Colours::Red, s.mask_color, Colours::Green);
	}; break;
	case ShaftValveStatus::Closed: {
		CAS_SLOT(s.body_color, Colours::Gray);
	}; break;
	case ShaftValveStatus::Closing: {
		CAS_SLOT(s.mask_color, Colours::DarkGray);
	}; break;
	case ShaftValveStatus::CloseReady: {
		CAS_VALUES(s.skeleton_color, Colours::Cyan, s.mask_color, Colours::DimGray);
	}; break;
	case ShaftValveStatus::Unclosable: {
		CAS_VALUES(s.skeleton_color, Colours::Red, s.mask_color, Colours::DarkGray);
	}; break;
	case ShaftValveStatus::FakeOpen: {
		CAS_VALUES(s.frame_color, Colours::Red, s.body_color, Colours::ForestGreen);
	}; break;
	case ShaftValveStatus::FakeClose: {
		CAS_VALUES(s.frame_color, Colours::Red, s.body_color, Colours::DimGray);
	}; break;
	}

	CAS_SLOT(s.skeleton_color, Colours::DarkGray);
	CAS_SLOT(s.body_color, Colours::Background);
	CAS_SLOT(s.shaft_color, Colours::Background);

	// NOTE: The others can be nullptr;
}

void ShaftValvelet::on_status_changed(ShaftValveStatus status) {
	double adjust_degrees = this->degrees + 90.0;

	switch (status) {
	case ShaftValveStatus::Unopenable: {
		if (this->bottom_up_mask == nullptr) {
			this->bottom_up_mask = polar_masked_sandglass(this->sgradius, adjust_degrees, -0.80);
		}
		this->mask = this->bottom_up_mask;
	} break;
	case ShaftValveStatus::Unclosable: case ShaftValveStatus::Disabled: {
		if (this->top_down_mask == nullptr) {
			this->top_down_mask = polar_masked_sandglass(this->sgradius, adjust_degrees, 0.80);
		}
		this->mask = this->top_down_mask;
	} break;
	case ShaftValveStatus::OpenReady: {
		if (this->bottom_up_ready_mask == nullptr) {
			this->bottom_up_ready_mask = polar_masked_sandglass(this->sgradius, adjust_degrees, -0.70);
		}
		this->mask = this->bottom_up_ready_mask;
	} break;
	case ShaftValveStatus::CloseReady: {
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

void ShaftValvelet::draw(CanvasDrawingSession^ ds, float x, float y, float Width, float Height) {
	const ShaftValveStyle style = this->get_style();
	float radius = this->size * 0.5F - default_thickness;
	float cx = x + radius + default_thickness;
	float cy = y + radius + default_thickness;
	
	if (style.shaft_color != Colours::Background) {
		ds->DrawGeometry(this->shaft, cx, cy, style.shaft_color, default_thickness);
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
