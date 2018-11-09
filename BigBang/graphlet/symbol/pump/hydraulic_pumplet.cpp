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
	: HydraulicPumplet(HydraulicPumpStatus::Stopped, radius, degrees) {}

HydraulicPumplet::HydraulicPumplet(HydraulicPumpStatus default_status, float radius, double degrees)
	: ISymbollet(default_status, radius, degrees) {
	this->tradius = radius - default_thickness * 2.0F;
}

void HydraulicPumplet::construct() {
	this->skeleton = polar_triangle(this->tradius, this->degrees);
	this->body = geometry_freeze(this->skeleton);
}

void HydraulicPumplet::update(long long count, long long interval, long long uptime) {
	double pmask = double(count % dynamic_mask_step) / double(dynamic_mask_step - 1);
	
	switch (this->get_status()) {
	case HydraulicPumpStatus::Starting: {
		this->mask = polar_masked_triangle(this->tradius, this->degrees, pmask);
		this->notify_updated();
	} break;
	case HydraulicPumpStatus::Stopping: {
		this->mask = polar_masked_triangle(this->tradius, this->degrees, 1.0 - pmask);
		this->notify_updated();
	} break;
	}
}

void HydraulicPumplet::on_status_changed(HydraulicPumpStatus status) {
	switch (status) {
	case HydraulicPumpStatus::StartReady: case HydraulicPumpStatus::Unstartable: {
		if (this->start_mask == nullptr) {
			this->start_mask = polar_masked_triangle(this->tradius, this->degrees, 0.382);
		}
		this->mask = this->start_mask;
	} break;
	case HydraulicPumpStatus::StopReady: case HydraulicPumpStatus::Unstoppable: {
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

void HydraulicPumplet::prepare_style(HydraulicPumpStatus status, HydraulicPumpStyle& s) {
	switch (status) {
	case HydraulicPumpStatus::Running: {
		CAS_SLOT(s.body_color, Colours::Green);
	}; break;
	case HydraulicPumpStatus::StartReady: {
		CAS_VALUES(s.body_color, Colours::DimGray, s.mask_color, Colours::Green);
		CAS_SLOT(s.skeleton_color, Colours::Cyan);
	}; break;
	case HydraulicPumpStatus::Starting: {
		CAS_VALUES(s.body_color, Colours::DimGray, s.mask_color, Colours::Green);
	}; break;
	case HydraulicPumpStatus::Unstartable: {
		CAS_VALUES(s.body_color, Colours::DimGray, s.mask_color, Colours::Green);
		CAS_SLOT(s.skeleton_color, Colours::Red);
	}; break;
	case HydraulicPumpStatus::StopReady: {
		CAS_SLOT(s.mask_color, Colours::ForestGreen);
		CAS_SLOT(s.skeleton_color, Colours::Cyan);
	}; break;
	case HydraulicPumpStatus::Stopping: {
		CAS_SLOT(s.mask_color, Colours::ForestGreen);
	}; break;
	case HydraulicPumpStatus::Unstoppable: {
		CAS_SLOT(s.mask_color, Colours::ForestGreen);
		CAS_SLOT(s.skeleton_color, Colours::Red);
	}; break;
	case HydraulicPumpStatus::Ready: {
		CAS_SLOT(s.skeleton_color, Colours::Cyan);
	}; break;
	case HydraulicPumpStatus::Broken: {
		CAS_SLOT(s.body_color, Colours::Red);
	}; break;
	}

	CAS_SLOT(s.remote_color, Colours::Cyan);
	CAS_SLOT(s.border_color, Colours::WhiteSmoke);
	CAS_SLOT(s.body_color, Colours::DarkGray);
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
	ds->DrawCachedGeometry(this->body, cx, cy, style.body_color);

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
	this->remote_control = on;
}
