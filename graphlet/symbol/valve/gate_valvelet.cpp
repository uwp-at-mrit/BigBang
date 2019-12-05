#include "graphlet/symbol/valve/gate_valvelet.hpp"

#include "datum/string.hpp"

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
static unsigned int dynamic_mask_step = 8;

/*************************************************************************************************/
GateValvelet::GateValvelet(float radius, double degrees)
	: GateValvelet(GateValveState::Default, radius, degrees) {}

GateValvelet::GateValvelet(GateValveState default_state, float radius, double degrees)
	: ISymbollet(default_state, radius, degrees), sgrdiff(default_thickness * 2.0F) {}

void GateValvelet::construct() {
	double adjust_degrees = this->degrees + 90.0;
	
	this->frame = polar_rectangle(this->radiusX, 60.0, adjust_degrees);
	this->skeleton = polar_sandglass(this->radiusX - this->sgrdiff, adjust_degrees);
	this->body = geometry_freeze(this->skeleton);
}

void GateValvelet::fill_margin(float x, float y, float* top, float* right, float* bottom, float* left) {
	GateValveStyle s = this->get_style();
	auto box = this->frame->ComputeStrokeBounds(default_thickness);
	
	SET_BOXES(left, right, (this->width - box.Width) * 0.5F);
	SET_BOXES(top, bottom, (this->height - box.Height) * 0.5F);
}

void GateValvelet::update(long long count, long long interval, long long uptime) {
	double pmask = double(count % dynamic_mask_step) / double(dynamic_mask_step - 1);
	double adjust_degrees = this->degrees + 90.0;
	float sandglass_r = this->radiusX - this->sgrdiff;

	switch (this->get_state()) {
	case GateValveState::Opening: {
		this->mask = polar_masked_sandglass(sandglass_r, adjust_degrees, -pmask);
		this->notify_updated();
	} break;
	case GateValveState::Closing: {
		this->mask = polar_masked_sandglass(sandglass_r, adjust_degrees, 1.0 - pmask);
		this->notify_updated();
	} break;
	}
}

void GateValvelet::prepare_style(GateValveState status, GateValveStyle& s) {
	switch (status) {
	case GateValveState::Default: {
		CAS_SLOT(s.mask_color, Colours::Teal);
	}; break;
	case GateValveState::Open: {
		CAS_SLOT(s.body_color, Colours::Green);
	}; break;
	case GateValveState::Opening: {
		CAS_SLOT(s.mask_color, Colours::Green);
	}; break;
	case GateValveState::OpenReady: {
		CAS_VALUES(s.skeleton_color, Colours::Cyan, s.mask_color, Colours::ForestGreen);
	}; break;
	case GateValveState::Unopenable: {
		CAS_VALUES(s.skeleton_color, Colours::Red, s.mask_color, Colours::Green);
	}; break;
	case GateValveState::Closed: {
		CAS_SLOT(s.body_color, Colours::Gray);
	}; break;
	case GateValveState::Closing: {
		CAS_SLOT(s.mask_color, Colours::DarkGray);
	}; break;
	case GateValveState::CloseReady: {
		CAS_VALUES(s.skeleton_color, Colours::Cyan, s.mask_color, Colours::DimGray);
	}; break;
	case GateValveState::Unclosable: {
		CAS_VALUES(s.skeleton_color, Colours::Red, s.mask_color, Colours::DarkGray);
	}; break;
	case GateValveState::Broken: {
		CAS_SLOT(s.body_color, Colours::Red);
	}; break;
	case GateValveState::VirtualOpen: {
		CAS_VALUES(s.frame_color, Colours::Red, s.body_color, Colours::ForestGreen);
	}; break;
	case GateValveState::VirtualClose: {
		CAS_VALUES(s.frame_color, Colours::Red, s.body_color, Colours::DimGray);
	}; break;
	case GateValveState::VirtualBroken: {
		CAS_SLOTS(s.frame_color, s.body_color, Colours::Red);
	}; break;
	}

	CAS_SLOT(s.skeleton_color, Colours::DarkGray);
	CAS_SLOT(s.body_color, Colours::Background);
	CAS_SLOT(s.frame_color, Colours::Background);

	// NOTE: The others can be nullptr;
}

void GateValvelet::on_state_changed(GateValveState status) {
	double adjust_degrees = this->degrees + 90.0;
	float sandglass_r = this->radiusX - this->sgrdiff;

	switch (status) {
	case GateValveState::Unopenable: {
		if (this->bottom_up_mask == nullptr) {
			this->bottom_up_mask = polar_masked_sandglass(sandglass_r, adjust_degrees, -0.80);
		}
		this->mask = this->bottom_up_mask;
	} break;
	case GateValveState::Unclosable: case GateValveState::Default: {
		if (this->top_down_mask == nullptr) {
			this->top_down_mask = polar_masked_sandglass(sandglass_r, adjust_degrees, 0.80);
		}
		this->mask = this->top_down_mask;
	} break;
	case GateValveState::OpenReady: {
		if (this->bottom_up_ready_mask == nullptr) {
			this->bottom_up_ready_mask = polar_masked_sandglass(sandglass_r, adjust_degrees, -0.70);
		}
		this->mask = this->bottom_up_ready_mask;
	} break;
	case GateValveState::CloseReady: {
		if (this->top_down_ready_mask == nullptr) {
			this->top_down_ready_mask = polar_masked_sandglass(sandglass_r, adjust_degrees, 0.70);
		}
		this->mask = this->top_down_ready_mask;
	} break;
	default: {
		this->mask = nullptr;
	}
	}
}

void GateValvelet::draw(CanvasDrawingSession^ ds, float x, float y, float Width, float Height) {
	const GateValveStyle style = this->get_style();
	float cx = x + this->radiusX;
	float cy = y + this->radiusY;
	
	if (style.frame_color != Colours::Background) {
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
