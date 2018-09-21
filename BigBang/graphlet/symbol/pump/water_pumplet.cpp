#include "graphlet/symbol/pump/water_pumplet.hpp"

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
WaterPumplet::WaterPumplet(float radius, double degrees)
	: WaterPumplet(WaterPumpStatus::Stopped, radius, degrees) {}

WaterPumplet::WaterPumplet(WaterPumpStatus default_status, float radius, double degrees)
	: ISymbollet(default_status, std::fabsf(radius), 0.0F, degrees, 1.0F), leftward(radius > 0.0F) {}

void WaterPumplet::construct() {
	float thickoff = default_thickness * 0.5F;
	float pump_radius = this->radiusX * 0.80F;
	float outlet_width = this->radiusX * 0.618F * 2.0F;
	float outlet_height = outlet_width * 0.618F;
	float outlet_extend = (this->radiusY - pump_radius) * 2.0F + outlet_height;
	float outlet_x = (this->leftward ? (-this->radiusX + thickoff) : (this->radiusX - outlet_width - thickoff));
	float outlet_y = -this->radiusY + (outlet_extend - outlet_height) * 0.5F + thickoff;
	float outlet_ex = (this->leftward ? outlet_x : (outlet_x + outlet_width));
	float outlet_ey = -this->radiusY + thickoff;
	float pump_cx = (this->radiusX - pump_radius - thickoff) * (this->leftward ? 1.0F : -1.0F);
	float pump_cy = this->radiusY - pump_radius - thickoff;
	float indicator_radius = pump_radius * 0.618F;
	auto pump = circle(pump_cx, pump_cy, pump_radius);
	auto outlet = rectangle(outlet_x, outlet_y, outlet_width, outlet_height);
	auto outlet_line = vline(outlet_ex, outlet_ey, outlet_extend);
	
	this->border = geometry_rotate(geometry_union(pump, geometry_union(outlet, outlet_line)), this->degrees, 0.0F, 0.0F);
	this->indicator = geometry_rotate(circle(pump_cx, pump_cy, indicator_radius), this->degrees, 0.0F, 0.0F);
	
	{ // locate
		auto box = this->indicator->ComputeBounds();

		this->pump_cx = box.X + box.Width * 0.5F;
		this->pump_cy = box.Y + box.Height * 0.5F;
		this->enclosing_box = this->border->ComputeStrokeBounds(default_thickness);
	}
}

void WaterPumplet::fill_margin(float x, float y, float* top, float* right, float* bottom, float* left) {
	float halfw = this->width * 0.5F;
	float halfh = this->height * 0.5F;

	SET_BOX(left, this->enclosing_box.X + halfw);
	SET_BOX(right, halfw - (this->enclosing_box.X + this->enclosing_box.Width));
	SET_BOX(top, this->enclosing_box.Y + halfh);
	SET_BOX(bottom, halfh - (this->enclosing_box.Y + this->enclosing_box.Height));
}

void WaterPumplet::fill_pump_origin(float* x, float *y) {
	SET_VALUES(x, this->pump_cx, y, this->pump_cy);
}

void WaterPumplet::prepare_style(WaterPumpStatus status, WaterPumpStyle& s) {
	switch (status) {
	case WaterPumpStatus::Running: {
		CAS_SLOT(s.body_color, Colours::Green);
		CAS_SLOT(s.skeleton_color, Colours::Green);
	}; break;
	case WaterPumpStatus::Unstartable: {
		CAS_SLOT(s.body_color, Colours::DimGray);
		CAS_SLOT(s.skeleton_color, Colours::Red);
	}; break;
	case WaterPumpStatus::Unstoppable: {
		CAS_SLOT(s.skeleton_color, Colours::Red);
	}; break;
	case WaterPumpStatus::Ready: {
		CAS_SLOT(s.skeleton_color, Colours::Cyan);
	}; break;
	}

	CAS_SLOT(s.remote_color, Colours::Cyan);
	CAS_SLOT(s.border_color, Colours::WhiteSmoke);
	CAS_SLOT(s.body_color, Colours::DarkGray);
	CAS_SLOT(s.skeleton_color, s.border_color);

	// NOTE: The others can be nullptr;
}

void WaterPumplet::draw(CanvasDrawingSession^ ds, float x, float y, float Width, float Height) {
	const WaterPumpStyle s = this->get_style();
	ICanvasBrush^ border_color = (this->remote_control ? s.remote_color : s.border_color);
	float cx = x + this->width * 0.5F;
	float cy = y + this->height * 0.5F;

	ds->FillGeometry(this->border, cx, cy, Colours::Background);
	ds->DrawGeometry(this->border, cx, cy, border_color, default_thickness);

	ds->FillGeometry(this->indicator, cx, cy, s.body_color);
	ds->DrawGeometry(this->indicator, cx, cy, s.skeleton_color, default_thickness);
}

void WaterPumplet::set_remote_control(bool on) {
	this->remote_control = on;
}
