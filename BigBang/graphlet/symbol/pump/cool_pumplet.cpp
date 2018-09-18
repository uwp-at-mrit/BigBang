#include "graphlet/symbol/pump/cool_pumplet.hpp"

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
CoolPumplet::CoolPumplet(float radiusX, float radiusY, double degrees)
	: CoolPumplet(CoolPumpStatus::Stopped, radiusX, radiusY, degrees) {}

CoolPumplet::CoolPumplet(CoolPumpStatus default_status, float radiusX, float radiusY, double degrees)
	: ISymbollet(default_status, std::fabsf(radiusX), std::fabsf(radiusY), degrees, 2.0F)
	, leftward(radiusX > 0.0F), upward(radiusY >= 0.0F) {}

void CoolPumplet::construct() {
	float thickoff = default_thickness * 0.5F;
	float inlet_height = this->radiusY * 0.5F;
	float inlet_width = inlet_height * 0.382F;
	float inlet_extend = inlet_height + inlet_width * 0.382F;
	float inlet_y = -inlet_height * 0.5F;
	float inlet_x = (this->leftward ? (-this->radiusX + thickoff) : (this->radiusX - inlet_width - thickoff));
	float inlet_ex = (this->leftward ? inlet_x : (inlet_x + inlet_width));
	float inlet_ey = -inlet_extend * 0.5F;
	float body_width = this->radiusX * 2.0F - default_thickness - inlet_width;
	float body_height = this->radiusY * 2.0F - default_thickness;
	float body_radius = body_width * 0.5F;
	float body_x = (this->leftward ? (-this->radiusX + inlet_width) : -this->radiusX) + thickoff;
	float body_y = -this->radiusY + thickoff;
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

	//ellipse_point(this->radiusX, this->radiusY, &this->pump_cx, &this->pump_cy);
	
	this->body = geometry_rotate(stadium, this->degrees, 0.0F, 0.0F);
	this->inlet = geometry_rotate(geometry_union(inlet, inlet_line), this->degrees, 0.0F, 0.0F);
	this->indicator = geometry_rotate(indicator, this->degrees, 0.0F, 0.0F);
	this->iborder = geometry_rotate(iborder, this->degrees, 0.0F, 0.0F);

	{ // locate
		auto box = this->body->ComputeBounds();

		this->pump_cx = box.X + box.Width * 0.5F;
		this->pump_cy = box.Y + box.Height * 0.5F;
		this->enclosing_box = geometry_union(this->body, this->inlet)->ComputeStrokeBounds(default_thickness);
	}
}

void CoolPumplet::fill_margin(float x, float y, float* top, float* right, float* bottom, float* left) {
	float halfw = this->width * 0.5F;
	float halfh = this->height * 0.5F;

	SET_BOX(left, this->enclosing_box.X + halfw);
	SET_BOX(right, halfw - (this->enclosing_box.X + this->enclosing_box.Width));
	SET_BOX(top, this->enclosing_box.Y + halfh);
	SET_BOX(bottom, halfh - (this->enclosing_box.Y + this->enclosing_box.Height));
}

void CoolPumplet::fill_pump_origin(float* x, float *y) {
	SET_VALUES(x, this->pump_cx, y, this->pump_cy);
}

void CoolPumplet::prepare_style(CoolPumpStatus status, CoolPumpStyle& s) {
	switch (status) {
	case CoolPumpStatus::Running: {
		CAS_SLOT(s.body_color, Colours::Green);
	}; break;
	case CoolPumpStatus::Unstartable: {
		CAS_SLOT(s.body_color, Colours::DimGray);
		CAS_SLOT(s.border_color, Colours::Firebrick);
	}; break;
	case CoolPumpStatus::Unstoppable: {
		CAS_SLOT(s.border_color, Colours::Firebrick);
	}; break;
	}

	CAS_SLOT(s.border_color, Colours::WhiteSmoke);
	CAS_SLOT(s.body_color, Colours::DarkGray);

	// NOTE: The others can be nullptr;
}

void CoolPumplet::draw(CanvasDrawingSession^ ds, float x, float y, float Width, float Height) {
	const CoolPumpStyle style = this->get_style();
	float cx = x + this->width * 0.5F;
	float cy = y + this->height * 0.5F;

	ds->FillGeometry(this->body, cx, cy, Colours::Background);
	ds->DrawGeometry(this->body, cx, cy, style.border_color, default_thickness);
	
	ds->FillGeometry(this->inlet, cx, cy, Colours::Background);
	ds->DrawGeometry(this->inlet, cx, cy, style.border_color, default_thickness);

	ds->FillGeometry(this->indicator, cx, cy, style.body_color);
	ds->DrawGeometry(this->indicator, cx, cy, style.border_color);
	ds->DrawGeometry(this->iborder, cx, cy, style.border_color, default_thickness);
}
