#include "graphlet/symbol/pump/hopper_pumplet.hpp"

#include "datum/flonum.hpp"

#include "polar.hpp"
#include "paint.hpp"
#include "shape.hpp"
#include "brushes.hxx"
#include "geometry.hpp"

using namespace WarGrey::SCADA;

using namespace Microsoft::Graphics::Canvas;
using namespace Microsoft::Graphics::Canvas::Brushes;

static float default_thickness = 2.0F;
static unsigned int dynamic_mask_step = 8U;

/*************************************************************************************************/
HopperPumplet::HopperPumplet(float radiusX, float radiusY, double degrees)
	: HopperPumplet(HopperPumpState::Stopped, radiusX, radiusY, degrees) {}

HopperPumplet::HopperPumplet(HopperPumpState default_state, float radiusX, float radiusY, double degrees)
	: ISymbollet(default_state, flabs(radiusX), flabs(radiusY), degrees, 2.0F)
	, leftward(radiusX > 0.0F), upward(radiusY >= 0.0F) {}

void HopperPumplet::construct() {
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

	this->border = geometry_rotate(stadium, this->degrees, 0.0F, 0.0F);
	this->inlet = geometry_rotate(geometry_union(inlet, inlet_line), this->degrees, 0.0F, 0.0F);
	this->skeleton = geometry_rotate(indicator, this->degrees, 0.0F, 0.0F);
	this->iborder = geometry_rotate(iborder, this->degrees, 0.0F, 0.0F);
	
	{ // make wrench
		TextExtent te;
		auto font = make_bold_text_format("Monospace", this->radiusX * 1.2F);
		auto wrshape = paragraph(L"🔧", font, &te);
		float wrcx = indicator_cx - te.width * 0.5F;
		float wrcy = -te.height * 0.5F;

		this->wrench = geometry_freeze(geometry_rotate(geometry_translate(wrshape, wrcx, wrcy), this->degrees, 0.0F, 0.0F));
	}

	{ // locate
		auto box = this->border->ComputeBounds();
		auto mask_box = this->skeleton->ComputeBounds();

		this->pump_cx = box.X + box.Width * 0.5F;
		this->pump_cy = box.Y + box.Height * 0.5F;
		this->enclosing_box = geometry_union(this->border, this->inlet)->ComputeStrokeBounds(default_thickness);

		this->iradius = indicator_radius;
		this->mask_cx = mask_box.X + mask_box.Width * 0.5F;
		this->mask_cy = mask_box.Y + mask_box.Height * 0.5F;
	}
}

void HopperPumplet::fill_margin(float x, float y, float* top, float* right, float* bottom, float* left) {
	float halfw = this->width * 0.5F;
	float halfh = this->height * 0.5F;

	SET_BOX(left, this->enclosing_box.X + halfw);
	SET_BOX(right, halfw - (this->enclosing_box.X + this->enclosing_box.Width));
	SET_BOX(top, this->enclosing_box.Y + halfh);
	SET_BOX(bottom, halfh - (this->enclosing_box.Y + this->enclosing_box.Height));
}

void HopperPumplet::fill_pump_origin(float* x, float *y) {
	SET_VALUES(x, this->pump_cx, y, this->pump_cy);
}

void HopperPumplet::update(long long count, long long interval, long long uptime) {
	double pmask = double(count % dynamic_mask_step) / double(dynamic_mask_step - 1);
	
	switch (this->get_state()) {
	case HopperPumpState::Starting: {
		this->mask = circle(this->mask_cx, this->mask_cy, this->iradius * float(pmask));
		this->notify_updated();
	} break;
	case HopperPumpState::Stopping: {
		this->mask = circle(this->mask_cx, this->mask_cy, this->iradius * float(1.0 - pmask));
		this->notify_updated();
	} break;
	}
}

void HopperPumplet::prepare_style(HopperPumpState status, HopperPumpStyle& s) {
	switch (status) {
	case HopperPumpState::Running: {
		CAS_SLOT(s.body_color, Colours::Green);
		CAS_SLOT(s.skeleton_color, Colours::Green);
	}; break;
	case HopperPumpState::StartReady: {
		CAS_VALUES(s.body_color, Colours::DimGray, s.mask_color, Colours::Green);
		CAS_SLOT(s.skeleton_color, Colours::Cyan);
	}; break;
	case HopperPumpState::Starting: {
		CAS_VALUES(s.body_color, Colours::DimGray, s.mask_color, Colours::Green);
	}; break;
	case HopperPumpState::Unstartable: {
		CAS_VALUES(s.body_color, Colours::DimGray, s.mask_color, Colours::Green);
		CAS_SLOT(s.skeleton_color, Colours::Red);
	}; break;
	case HopperPumpState::StopReady: {
		CAS_SLOT(s.mask_color, Colours::ForestGreen);
		CAS_SLOT(s.skeleton_color, Colours::Cyan);
	}; break;
	case HopperPumpState::Stopping: {
		CAS_SLOT(s.mask_color, Colours::ForestGreen);
	}; break;
	case HopperPumpState::Unstoppable: {
		CAS_SLOT(s.mask_color, Colours::ForestGreen);
		CAS_SLOT(s.skeleton_color, Colours::Red);
	}; break;
	case HopperPumpState::Ready: {
		CAS_SLOT(s.skeleton_color, Colours::Cyan);
	}; break;
	case HopperPumpState::Broken: case HopperPumpState::Alert: {
		CAS_SLOT(s.body_color, Colours::Red);
	}; break;
	case HopperPumpState::Maintenance: {
		CAS_SLOT(s.wrench_color, Colours::Red);
	}; break;
	}

	CAS_SLOT(s.remote_color, Colours::Cyan);
	CAS_SLOT(s.border_color, Colours::WhiteSmoke);
	CAS_SLOT(s.body_color, Colours::DarkGray);
	CAS_SLOT(s.skeleton_color, s.body_color);

	// NOTE: The others can be nullptr;
}

void HopperPumplet::on_state_changed(HopperPumpState status) {
	switch (status) {
	case HopperPumpState::StartReady: case HopperPumpState::Unstartable: {
		if (this->start_mask == nullptr) {
			this->start_mask = circle(this->mask_cx, this->mask_cy, this->iradius * 0.382F);
		}
		this->mask = this->start_mask;
	} break;
	case HopperPumpState::StopReady: case HopperPumpState::Unstoppable: {
		if (this->stop_mask == nullptr) {
			this->stop_mask = circle(this->mask_cx, this->mask_cy, this->iradius * 0.618F);
		}
		this->mask = this->stop_mask;
	} break;
	default: {
		this->mask = nullptr;
	}
	}
}

void HopperPumplet::draw(CanvasDrawingSession^ ds, float x, float y, float Width, float Height) {
	const HopperPumpStyle s = this->get_style();
	ICanvasBrush^ border_color = (this->remote_control ? s.remote_color : s.border_color);
	float cx = x + this->width * 0.5F;
	float cy = y + this->height * 0.5F;

	ds->FillGeometry(this->border, cx, cy, Colours::Background);
	ds->DrawGeometry(this->border, cx, cy, border_color, default_thickness);

	ds->FillGeometry(this->inlet, cx, cy, Colours::Background);
	ds->DrawGeometry(this->inlet, cx, cy, border_color, default_thickness);

	ds->FillGeometry(this->skeleton, cx, cy, s.body_color);

	if (s.mask_color != nullptr) {
		auto mask = ((this->mask == nullptr) ? this->skeleton : this->mask);

		ds->FillGeometry(mask, cx, cy, s.mask_color);
	}

	ds->DrawGeometry(this->skeleton, cx, cy, s.skeleton_color, default_thickness);
	ds->DrawGeometry(this->iborder, cx, cy, border_color, default_thickness);

	if (s.wrench_color != nullptr) {
		ds->DrawCachedGeometry(this->wrench, cx, cy, s.wrench_color);
	}
}

void HopperPumplet::set_remote_control(bool on) {
	if (this->remote_control != on) {
		this->remote_control = on;
		this->notify_updated();
	}
}
