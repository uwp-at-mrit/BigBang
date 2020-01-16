#include "graphlet/symbol/pump/hydraulic_pumplet.hpp"

#include "polar.hpp"
#include "paint.hpp"
#include "brushes.hxx"
#include "geometry.hpp"

using namespace WarGrey::SCADA;

using namespace Microsoft::Graphics::Canvas;
using namespace Microsoft::Graphics::Canvas::Brushes;

static float default_thickness = 2.0F;
static unsigned int dynamic_mask_step = 8U;

/*************************************************************************************************/
HydraulicPumplet::HydraulicPumplet(float radius, double degrees)
	: HydraulicPumplet(HydraulicPumpState::Stopped, radius, degrees) {}

HydraulicPumplet::HydraulicPumplet(HydraulicPumpState default_state, float radius, double degrees)
	: ISymbollet(default_state, radius, degrees) {
	this->tradius = radius - default_thickness * 2.0F;
}

void HydraulicPumplet::construct() {
	this->skeleton = polar_triangle(this->tradius, this->degrees);
	this->body = geometry_freeze(this->skeleton);
}

void HydraulicPumplet::update(long long count, long long interval, long long uptime) {
	double pmask = double(count % dynamic_mask_step) / double(dynamic_mask_step - 1);
	
	switch (this->get_state()) {
	case HydraulicPumpState::Starting: {
		this->mask = polar_masked_triangle(this->tradius, this->degrees, pmask);
		this->notify_updated();
	} break;
	case HydraulicPumpState::Stopping: {
		this->mask = polar_masked_triangle(this->tradius, this->degrees, 1.0 - pmask);
		this->notify_updated();
	} break;
	}
}

void HydraulicPumplet::on_state_changed(HydraulicPumpState status) {
	switch (status) {
	case HydraulicPumpState::StartReady: case HydraulicPumpState::Unstartable: {
		if (this->start_mask == nullptr) {
			this->start_mask = polar_masked_triangle(this->tradius, this->degrees, 0.382);
		}
		this->mask = this->start_mask;
	} break;
	case HydraulicPumpState::StopReady: case HydraulicPumpState::Unstoppable: {
		if (this->stop_mask == nullptr) {
			this->stop_mask = polar_masked_triangle(this->tradius, this->degrees, 0.618);
		}
		this->mask = this->stop_mask;
	} break;
	default: {
		this->mask = nullptr;
	}
	}
}

void HydraulicPumplet::prepare_style(HydraulicPumpState status, HydraulicPumpStyle& s) {
	switch (status) {
	case HydraulicPumpState::Running: {
		CAS_SLOT(s.body_color, Colours::Green);
	}; break;
	case HydraulicPumpState::StartReady: {
		CAS_VALUES(s.body_color, Colours::DimGray, s.mask_color, Colours::Green);
		CAS_SLOT(s.skeleton_color, Colours::Cyan);
	}; break;
	case HydraulicPumpState::Starting: {
		CAS_VALUES(s.body_color, Colours::DimGray, s.mask_color, Colours::Green);
	}; break;
	case HydraulicPumpState::Unstartable: {
		CAS_VALUES(s.body_color, Colours::DimGray, s.mask_color, Colours::Green);
		CAS_SLOT(s.skeleton_color, Colours::Red);
	}; break;
	case HydraulicPumpState::StopReady: {
		CAS_SLOT(s.mask_color, Colours::ForestGreen);
		CAS_SLOT(s.skeleton_color, Colours::Cyan);
	}; break;
	case HydraulicPumpState::Stopping: {
		CAS_SLOT(s.mask_color, Colours::ForestGreen);
	}; break;
	case HydraulicPumpState::Unstoppable: {
		CAS_SLOT(s.mask_color, Colours::ForestGreen);
		CAS_SLOT(s.skeleton_color, Colours::Red);
	}; break;
	case HydraulicPumpState::Ready: {
		CAS_SLOT(s.skeleton_color, Colours::Cyan);
	}; break;
	case HydraulicPumpState::Broken: {
		CAS_SLOT(s.body_color, Colours::Red);
	}; break;
	}

	CAS_SLOT(s.remote_color, Colours::Cyan);
	CAS_SLOT(s.border_color, Colours::WhiteSmoke);
	CAS_SLOT(s.body_color, Colours::DarkGray);
	CAS_SLOT(s.auto_color, Colours::RoyalBlue);
	CAS_SLOT(s.skeleton_color, s.body_color);

	// NOTE: The others can be nullptr;
}

void HydraulicPumplet::draw(CanvasDrawingSession^ ds, float x, float y, float Width, float Height) {
	const HydraulicPumpStyle style = this->get_style();
	float radiusX = this->width * 0.5F - default_thickness;
	float radiusY = this->height * 0.5F - default_thickness;
	float cx = x + radiusX + default_thickness;
	float cy = y + radiusY + default_thickness;
	
	ds->FillEllipse(cx, cy, radiusX, radiusY, Colours::Background);
	ds->DrawCachedGeometry(this->body, cx, cy, (this->auto_mode ? style.auto_color : style.body_color));

	if (style.mask_color != nullptr) {
		auto mask = ((this->mask == nullptr) ? this->skeleton : this->mask);
		
		ds->FillGeometry(mask, cx, cy, style.mask_color);
	}

	ds->DrawGeometry(this->skeleton, cx, cy, style.skeleton_color, default_thickness);

	if (this->remote_control) {
		ds->DrawEllipse(cx, cy, radiusX, radiusY, style.remote_color, default_thickness);
	} else {
		ds->DrawEllipse(cx, cy, radiusX, radiusY, style.border_color, default_thickness);
	}
}

void HydraulicPumplet::set_remote_control(bool on) {
	if (this->remote_control != on) {
		this->remote_control = on;
		this->notify_updated();
	}
}

void HydraulicPumplet::set_auto_mode(bool on) {
	if (this->auto_mode != on) {
		this->auto_mode = on;
		this->notify_updated();
	}
}

bool HydraulicPumplet::is_auto() {
	return this->auto_mode;
}
