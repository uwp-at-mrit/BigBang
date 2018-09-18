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
CoolPumplet::CoolPumplet(float radius, double degrees)
	: CoolPumplet(CoolPumpStatus::Stopped, radius, degrees) {}

CoolPumplet::CoolPumplet(CoolPumpStatus default_status, float radius, double degrees)
	: ISymbollet(default_status, std::fabsf(radius), 0.0F, degrees, 1.0F), leftward(radius > 0.0F) {}

void CoolPumplet::construct() {
	float thickoff = default_thickness * 0.5F;
	float pump_radius = this->radiusX * 0.80F;
	float inlet_width = this->radiusX * 0.618F * 2.0F;
	float inlet_height = inlet_width * 0.618F;
	float inlet_extend = (this->radiusY - pump_radius) * 2.0F + inlet_height;
	float inlet_x = (this->leftward ? (-this->radiusX + thickoff) : (this->radiusX - inlet_width - thickoff));
	float inlet_y = -this->radiusY + (inlet_extend - inlet_height) * 0.5F + thickoff;
	float inlet_ex = (this->leftward ? inlet_x : (inlet_x + inlet_width));
	float inlet_ey = -this->radiusY + thickoff;
	float pump_cx = (this->radiusX - pump_radius - thickoff) * (this->leftward ? 1.0F : -1.0F);
	float pump_cy = this->radiusY - pump_radius - thickoff;
	float indicator_radius = pump_radius * 0.618F;
	auto pump = circle(pump_cx, pump_cy, pump_radius);
	auto inlet = rectangle(inlet_x, inlet_y, inlet_width, inlet_height);
	auto inlet_line = vline(inlet_ex, inlet_ey, inlet_extend);
	
	this->border = geometry_rotate(geometry_union(pump, geometry_union(inlet, inlet_line)), this->degrees, 0.0F, 0.0F);
	this->indicator = geometry_rotate(circle(pump_cx, pump_cy, indicator_radius), this->degrees, 0.0F, 0.0F);
	
	{ // locate
		auto box = this->indicator->ComputeBounds();

		this->pump_cx = box.X + box.Width * 0.5F;
		this->pump_cy = box.Y + box.Height * 0.5F;
		this->enclosing_box = this->border->ComputeStrokeBounds(default_thickness);
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

	ds->FillGeometry(this->border, cx, cy, Colours::Background);
	ds->DrawGeometry(this->border, cx, cy, style.border_color, default_thickness);

	ds->FillGeometry(this->indicator, cx, cy, style.body_color);
	ds->DrawGeometry(this->indicator, cx, cy, style.border_color);
}
