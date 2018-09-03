#include "graphlet/symbol/pumplet.hpp"

#include "polar.hpp"
#include "paint.hpp"
#include "brushes.hxx"
#include "geometry.hpp"

using namespace WarGrey::SCADA;

using namespace Microsoft::Graphics::Canvas;
using namespace Microsoft::Graphics::Canvas::Brushes;

static float default_thickness = 2.0F;
static double dynamic_mask_interval = 1.0 / 8.0;

/*************************************************************************************************/
HydraulicPumplet::HydraulicPumplet(float radius, double degrees) : HydraulicPumplet(PumpStatus::Stopped, radius, degrees) {}

HydraulicPumplet::HydraulicPumplet(PumpStatus default_status, float radius, double degrees) : ISymbollet(default_status, radius, degrees) {
	this->tradius = radius - default_thickness * 2.0F;
}

void HydraulicPumplet::construct() {
	this->skeleton = polar_triangle(this->tradius, this->degrees);
	this->body = geometry_freeze(this->skeleton);
}

void HydraulicPumplet::update(long long count, long long interval, long long uptime) {
	switch (this->get_status()) {
	case PumpStatus::Starting: {
		this->mask_percentage
			= ((this->mask_percentage < 0.0) || (this->mask_percentage >= 1.0))
			? 0.0
			: this->mask_percentage + dynamic_mask_interval;

		this->mask = polar_masked_triangle(this->tradius, this->degrees, this->mask_percentage);
		this->notify_updated();
	} break;
	case PumpStatus::Stopping: {
		this->mask_percentage
			= ((this->mask_percentage <= 0.0) || (this->mask_percentage > 1.0))
			? 1.0
			: this->mask_percentage - dynamic_mask_interval;

		this->mask = polar_masked_triangle(this->tradius, this->degrees, this->mask_percentage);
		this->notify_updated();
	} break;
	}
}

void HydraulicPumplet::on_status_changed(PumpStatus status) {
	switch (status) {
	case PumpStatus::Unstartable: {
		if (this->unstartable_mask == nullptr) {
			this->unstartable_mask = polar_masked_triangle(this->tradius, this->degrees, 0.382);
		}
		this->mask = this->unstartable_mask;
	} break;
	case PumpStatus::Unstoppable: {
		if (this->unstoppable_mask == nullptr) {
			this->unstoppable_mask = polar_masked_triangle(this->tradius, this->degrees, 0.618);
		}
		this->mask = this->unstoppable_mask;
	} break;
	default: {
		this->mask = nullptr;
		this->mask_percentage = -1.0;
	}
	}
}

void HydraulicPumplet::prepare_style(PumpStatus status, PumpStyle& s) {
	switch (status) {
	case PumpStatus::Running: {
		CAS_SLOT(s.body_color, Colours::Green);
	}; break;
	case PumpStatus::Starting: {
		CAS_VALUES(s.body_color, Colours::DimGray, s.mask_color, Colours::Green);
	}; break;
	case PumpStatus::Unstartable: {
		CAS_VALUES(s.body_color, Colours::DimGray, s.mask_color, Colours::Green);
		CAS_SLOT(s.border_color, Colours::Firebrick);
	}; break;
	case PumpStatus::Remote: {
		CAS_SLOT(s.border_color, Colours::Cyan);
	}; break;
	case PumpStatus::Stopping: {
		CAS_SLOT(s.mask_color, Colours::ForestGreen);
	}; break;
	case PumpStatus::Unstoppable: {
		CAS_VALUES(s.mask_color, Colours::ForestGreen, s.border_color, Colours::Firebrick);
	}; break;
	case PumpStatus::Ready: {
		CAS_SLOT(s.skeleton_color, Colours::Cyan);
	}; break;
	}

	CAS_SLOT(s.border_color, Colours::WhiteSmoke);
	CAS_SLOT(s.body_color, Colours::DarkGray);
	CAS_SLOT(s.skeleton_color, s.body_color);

	// NOTE: The others can be nullptr;
}

void HydraulicPumplet::draw(CanvasDrawingSession^ ds, float x, float y, float Width, float Height) {
	const PumpStyle style = this->get_style();
	float radius = this->size * 0.5F - default_thickness;
	float cx = x + radius + default_thickness;
	float cy = y + radius + default_thickness;
	
	ds->FillCircle(cx, cy, radius, Colours::Background);
	ds->DrawCircle(cx, cy, radius, style.border_color, default_thickness);
	ds->DrawCachedGeometry(this->body, cx, cy, style.body_color);
	ds->DrawGeometry(this->skeleton, cx, cy, style.skeleton_color, default_thickness);

	if (style.mask_color != nullptr) {
		auto mask = ((this->mask == nullptr) ? this->skeleton : this->mask);
		
		ds->FillGeometry(mask, cx, cy, style.mask_color);
		ds->DrawGeometry(mask, cx, cy, style.mask_color, default_thickness);
	}
}
