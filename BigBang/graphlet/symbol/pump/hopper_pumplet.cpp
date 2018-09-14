#include "graphlet/symbol/pump/hopper_pumplet.hpp"

#include "polar.hpp"
#include "paint.hpp"
#include "shape.hpp"
#include "brushes.hxx"
#include "geometry.hpp"

using namespace WarGrey::SCADA;

using namespace Microsoft::Graphics::Canvas;
using namespace Microsoft::Graphics::Canvas::Brushes;

static float default_thickness = 2.0F;
static double dynamic_mask_interval = 1.0 / 8.0;

/*************************************************************************************************/
HopperPumplet::HopperPumplet(float radiusX, float radiusY, double degrees)
	: HopperPumplet(HopperPumpStatus::Stopped, radiusX, radiusY, degrees) {}

HopperPumplet::HopperPumplet(HopperPumpStatus default_status, float radiusX, float radiusY, double degrees)
	: ISymbollet(default_status, std::fabsf(radiusX), radiusY, degrees), leftward(radiusX > 0.0F) {}

void HopperPumplet::construct() {
	float thickoff = default_thickness * 0.5F;
	float body_width = this->radiusX * 2.0F - default_thickness;
	float body_height = this->radiusY * 2.0F - default_thickness;
	float body_x = (this->leftward ? (this->radiusX * 2.0F - body_width - thickoff) : thickoff);
	float body_radius = body_width * 0.5F;
	float indicator_radius = body_radius * 0.618F;
	
	this->body = geometry_rotate(rounded_rectangle(0.0F, 0.0F, body_width, body_height, body_radius, body_radius), this->degrees);
	this->indicator = circle(0.0F, 0.0F, indicator_radius);
}

void HopperPumplet::update(long long count, long long interval, long long uptime) {
}

void HopperPumplet::on_status_changed(HopperPumpStatus status) {
	//switch (status) {
	//default: {
		this->mask = nullptr;
		this->mask_percentage = -1.0;
	//}
	//}
}

void HopperPumplet::prepare_style(HopperPumpStatus status, HopperPumpStyle& s) {
	switch (status) {
	case HopperPumpStatus::Running: {
		CAS_SLOT(s.body_color, Colours::Green);
	}; break;
	case HopperPumpStatus::Starting: {
		CAS_VALUES(s.body_color, Colours::DimGray, s.mask_color, Colours::Green);
	}; break;
	case HopperPumpStatus::Unstartable: {
		CAS_VALUES(s.body_color, Colours::DimGray, s.mask_color, Colours::Green);
		CAS_SLOT(s.border_color, Colours::Firebrick);
	}; break;
	case HopperPumpStatus::Remote: {
		CAS_SLOT(s.border_color, Colours::Cyan);
	}; break;
	case HopperPumpStatus::Stopping: {
		CAS_SLOT(s.mask_color, Colours::ForestGreen);
	}; break;
	case HopperPumpStatus::Unstoppable: {
		CAS_VALUES(s.mask_color, Colours::ForestGreen, s.border_color, Colours::Firebrick);
	}; break;
	case HopperPumpStatus::Ready: {
		CAS_SLOT(s.skeleton_color, Colours::Cyan);
	}; break;
	}

	CAS_SLOT(s.border_color, Colours::WhiteSmoke);
	CAS_SLOT(s.body_color, Colours::DarkGray);
	CAS_SLOT(s.skeleton_color, s.body_color);

	// NOTE: The others can be nullptr;
}

void HopperPumplet::draw(CanvasDrawingSession^ ds, float x, float y, float Width, float Height) {
	const HopperPumpStyle style = this->get_style();
	float cx = this->width * 2.0F;
	float cy = this->height * 2.0F;
	
	ds->FillGeometry(this->body, cx, cy, Colours::Background);
	ds->DrawGeometry(this->body, cx, cy, style.border_color, default_thickness);

	ds->FillGeometry(this->indicator, cx, cy, style.body_color);
	ds->DrawGeometry(this->indicator, cx, cy, style.border_color);
}
