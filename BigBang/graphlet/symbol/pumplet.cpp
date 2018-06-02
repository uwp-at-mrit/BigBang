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

static PumpStatus default_pump_status = PumpStatus::Stopped;
static CanvasSolidColorBrush^ default_body_color = Colours::DarkGray;
static CanvasSolidColorBrush^ default_border_color = Colours::WhiteSmoke;

PumpStyle WarGrey::SCADA::make_default_pump_style(PumpStatus status) {
	PumpStyle s;

	s.border_color = default_border_color;
	s.body_color = default_body_color;

	switch (status) {
	case PumpStatus::Running: s.body_color = Colours::Green; break;
	case PumpStatus::Starting: s.body_color = Colours::DimGray; s.mask_color = Colours::Green; break;
	case PumpStatus::Unstartable: s.body_color = Colours::DimGray; s.mask_color = Colours::Green; break;
	case PumpStatus::Remote: s.border_color = Colours::Cyan; break;
	case PumpStatus::Stopped: break; // this is the default to draw
	case PumpStatus::Stopping: s.mask_color = Colours::ForestGreen; break;
	case PumpStatus::Unstoppable: s.mask_color = Colours::ForestGreen; break;
	case PumpStatus::Ready: s.skeleton_color = Colours::Cyan; break;
	}

	return s;
}

/*************************************************************************************************/
Pumplet::Pumplet(float radius, double degrees) : Pumplet(default_pump_status, radius, degrees) {}

Pumplet::Pumplet(PumpStatus default_status, float radius, double degrees)
	: ISymbollet(default_status, &make_default_pump_style, radius, degrees) {
	this->tradius = radius - default_thickness * 2.0F;
	this->on_status_change(default_status);
}

void Pumplet::construct() {
	this->skeleton = polar_triangle(this->tradius, this->degrees);
	this->body = geometry_freeze(this->skeleton);
}

void Pumplet::update(long long count, long long interval, long long uptime) {
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

void Pumplet::on_status_change(PumpStatus status) {
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

void Pumplet::draw(CanvasDrawingSession^ ds, float x, float y, float Width, float Height) {
	const PumpStyle style = this->get_style();
	auto body_color = (style.body_color != nullptr) ? style.body_color : default_body_color;
	auto skeleton_color = (style.skeleton_color != nullptr) ? style.skeleton_color : body_color;
	auto border_color = (style.border_color != nullptr) ? style.border_color : default_border_color;

	float radius = this->size * 0.5F - default_thickness;
	float cx = x + radius + default_thickness;
	float cy = y + radius + default_thickness;
	
	ds->FillCircle(cx, cy, radius, Colours::Background);
	ds->DrawCircle(cx, cy, radius, border_color, default_thickness);
	ds->DrawCachedGeometry(this->body, cx, cy, body_color);
	ds->DrawGeometry(this->skeleton, cx, cy, skeleton_color, default_thickness);

	if (style.mask_color != nullptr) {
		auto mask = ((this->mask == nullptr) ? this->skeleton : this->mask);
		
		ds->FillGeometry(mask, cx, cy, style.mask_color);
		ds->DrawGeometry(mask, cx, cy, style.mask_color, default_thickness);
	}
}
