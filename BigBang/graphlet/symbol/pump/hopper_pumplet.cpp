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
	: ISymbollet(default_status, std::fabsf(radiusX), std::fabsf(radiusY), degrees, 2.0F)
	, leftward(radiusX > 0.0F), upward(radiusY >= 0.0F) {}

void HopperPumplet::construct() {
	float thickoff = default_thickness * 0.5F;
	float inlet_height = this->radiusY * 0.5F;
	float inlet_width = inlet_height * 0.382F;
	float inlet_extend = inlet_height + inlet_width * 0.382F;
	float inlet_y = (this->radiusY * 2.0F - inlet_height) * 0.5F;
	float inlet_x = (this->leftward ? thickoff : (this->radiusX * 2.0F - inlet_width - thickoff));
	float inlet_ex = (this->leftward ? thickoff : (this->radiusX * 2.0F - thickoff));
	float inlet_ey = inlet_y - (inlet_extend - inlet_height) * 0.5F;
	float body_width = this->radiusX * 2.0F - default_thickness - inlet_width;
	float body_height = this->radiusY * 2.0F - default_thickness;
	float body_radius = body_width * 0.5F;
	float body_x = (this->leftward ? (thickoff + inlet_width) : thickoff);
	float body_y = thickoff;
	float irdiff = default_thickness * 2.0F;
	float indicator_radius = body_radius - irdiff;
	float icydiff = irdiff + indicator_radius;
	float indicator_cx = body_x + irdiff + indicator_radius;
	float indicator_cy = (this->upward ? (body_y + icydiff) : (body_y + body_height - icydiff));
	auto stadium = rounded_rectangle(body_x, body_y, body_width, body_height, body_radius, body_radius);
	auto inlet = rectangle(inlet_x, inlet_y, inlet_width, inlet_height);
	auto inlet_line = vline(inlet_ex, inlet_ey, inlet_extend);
	auto indicator = circle(indicator_cx, indicator_cy, indicator_radius);
	auto iborder = circle(indicator_cx, indicator_cy, body_radius);
	
	this->body = geometry_rotate(stadium, this->degrees, this->radiusX, this->radiusY);
	this->inlet = geometry_rotate(geometry_union(inlet, inlet_line), this->degrees, this->radiusX, this->radiusY);
	this->indicator = geometry_rotate(indicator, this->degrees, this->radiusX, this->radiusY);
	this->iborder = geometry_rotate(iborder, this->degrees, this->radiusX, this->radiusY);
}

void HopperPumplet::prepare_style(HopperPumpStatus status, HopperPumpStyle& s) {
	switch (status) {
	case HopperPumpStatus::Running: {
		CAS_SLOT(s.body_color, Colours::Green);
	}; break;
	case HopperPumpStatus::Unstartable: {
		CAS_SLOT(s.body_color, Colours::DimGray);
		CAS_SLOT(s.border_color, Colours::Firebrick);
	}; break;
	case HopperPumpStatus::Unstoppable: {
		CAS_SLOT(s.border_color, Colours::Firebrick);
	}; break;
	}

	CAS_SLOT(s.border_color, Colours::WhiteSmoke);
	CAS_SLOT(s.body_color, Colours::DarkGray);

	// NOTE: The others can be nullptr;
}

void HopperPumplet::draw(CanvasDrawingSession^ ds, float x, float y, float Width, float Height) {
	const HopperPumpStyle style = this->get_style();
	
	ds->FillGeometry(this->body, x, y, Colours::Background);
	ds->DrawGeometry(this->body, x, y, style.border_color, default_thickness);
	
	ds->FillGeometry(this->inlet, x, y, Colours::Background);
	ds->DrawGeometry(this->inlet, x, y, style.border_color, default_thickness);

	ds->FillGeometry(this->indicator, x, y, style.body_color);
	ds->DrawGeometry(this->indicator, x, y, style.border_color);
	ds->DrawGeometry(this->iborder, x, y, style.border_color, default_thickness);
}
