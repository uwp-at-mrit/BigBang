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

/*************************************************************************************************/
ManualValvelet::ManualValvelet(float radius, double degrees)
	: ManualValvelet(ManualValveState::OpenReady, radius, degrees) {}

ManualValvelet::ManualValvelet(ManualValveState default_state, float radius, double degrees)
	: ISymbollet(default_state, radius, degrees) {}

void ManualValvelet::construct() {
	double adjust_degrees = this->degrees + 90.0;
	float stem_length = this->radiusX * 0.618F;
	auto stem = polar_axis(stem_length, this->degrees - 90.0);
	auto wheel = polar_pole(stem_length, this->degrees - 90.0, stem_length * 0.1618F);

	this->stem = geometry_union(stem, wheel);
	this->skeleton = polar_sandglass(this->radiusX, adjust_degrees);
	this->body = geometry_freeze(this->skeleton);
}

void ManualValvelet::fill_margin(float x, float y, float* top, float* right, float* bottom, float* left) {
	ManualValveStyle s = this->get_style();
	auto box = this->skeleton->ComputeStrokeBounds(default_thickness);
	auto stem_box = this->stem->ComputeStrokeBounds(default_thickness);
	float hs = (this->width - box.Width) * 0.5F;
	float vs = (this->height - box.Height) * 0.5F;
	
	SET_BOX(left, fminf(hs, stem_box.X + this->radiusX));
	SET_BOX(right, fminf(hs, this->radiusX - (stem_box.X + stem_box.Width)));
	SET_BOX(top, fminf(vs, stem_box.Y + this->radiusY));
	SET_BOX(bottom, fminf(vs, this->radiusY - (stem_box.Y + stem_box.Height)));
}

void ManualValvelet::prepare_style(ManualValveState status, ManualValveStyle& s) {
	switch (status) {
	case ManualValveState::Default: {
		CAS_SLOT(s.mask_color, Colours::Teal);
	}; break;
	case ManualValveState::Open: {
		CAS_SLOT(s.body_color, Colours::Green);
	}; break;
	case ManualValveState::OpenReady: {
		CAS_VALUES(s.skeleton_color, Colours::Cyan, s.mask_color, Colours::ForestGreen);
	}; break;
	case ManualValveState::Unopenable: {
		CAS_VALUES(s.skeleton_color, Colours::Red, s.mask_color, Colours::Green);
	}; break;
	case ManualValveState::Closed: {
		CAS_SLOT(s.body_color, Colours::Gray);
	}; break;
	case ManualValveState::CloseReady: {
		CAS_VALUES(s.skeleton_color, Colours::Cyan, s.mask_color, Colours::DimGray);
	}; break;
	case ManualValveState::Unclosable: {
		CAS_VALUES(s.skeleton_color, Colours::Red, s.mask_color, Colours::DarkGray);
	}; break;
	}

	CAS_SLOT(s.skeleton_color, Colours::DarkGray);
	CAS_SLOT(s.stem_color, s.skeleton_color);
	CAS_SLOT(s.body_color, Colours::Background);

	// NOTE: The others can be nullptr;
}

void ManualValvelet::on_state_changed(ManualValveState status) {
	double adjust_degrees = this->degrees + 90.0;
	float sandglass_r = this->radiusX;

	switch (status) {
	case ManualValveState::Unopenable: {
		if (this->bottom_up_mask == nullptr) {
			this->bottom_up_mask = polar_masked_sandglass(sandglass_r, adjust_degrees, -0.80);
		}
		this->mask = this->bottom_up_mask;
	} break;
	case ManualValveState::Unclosable: case ManualValveState::Default: {
		if (this->top_down_mask == nullptr) {
			this->top_down_mask = polar_masked_sandglass(sandglass_r, adjust_degrees, 0.80);
		}
		this->mask = this->top_down_mask;
	} break;
	case ManualValveState::OpenReady: {
		if (this->bottom_up_ready_mask == nullptr) {
			this->bottom_up_ready_mask = polar_masked_sandglass(sandglass_r, adjust_degrees, -0.70);
		}
		this->mask = this->bottom_up_ready_mask;
	} break;
	case ManualValveState::CloseReady: {
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

void ManualValvelet::draw(CanvasDrawingSession^ ds, float x, float y, float Width, float Height) {
	const ManualValveStyle style = this->get_style();
	float cx = x + this->radiusX;
	float cy = y + this->radiusY;
	
	ds->DrawGeometry(this->stem, cx, cy, style.stem_color, default_thickness);
	ds->DrawCachedGeometry(this->body, cx, cy, style.body_color);

	if (style.mask_color != nullptr) {
		auto mask = ((this->mask == nullptr) ? this->skeleton : this->mask);
		
		ds->FillGeometry(mask, cx, cy, style.mask_color);
		ds->DrawGeometry(mask, cx, cy, style.mask_color, default_thickness);
	}

	ds->DrawGeometry(this->skeleton, cx, cy, style.skeleton_color, default_thickness);
}
