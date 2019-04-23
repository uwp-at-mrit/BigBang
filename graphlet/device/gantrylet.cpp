#include "graphlet/device/gantrylet.hpp"

#include "math.hpp"
#include "text.hpp"
#include "polar.hpp"
#include "shape.hpp"
#include "paint.hpp"
#include "geometry.hpp"
#include "colorspace.hpp"

#include "measure/vhatchmark.hpp"
#include "datum/flonum.hpp"

using namespace WarGrey::SCADA;

using namespace Windows::Foundation;
using namespace Windows::Foundation::Numerics;

using namespace Microsoft::Graphics::Canvas;
using namespace Microsoft::Graphics::Canvas::Text;
using namespace Microsoft::Graphics::Canvas::Brushes;
using namespace Microsoft::Graphics::Canvas::Geometry;

static CanvasSolidColorBrush^ gantry_default_color = Colours::Gold;
static CanvasSolidColorBrush^ gantry_default_pulley_color = Colours::DimGray;
static CanvasSolidColorBrush^ gantry_default_progress_color = Colours::ForestGreen;

static CanvasGeometry^ make_pivot_base(float radius, float extent_ratio, bool leftward) {
	auto base = ref new CanvasPathBuilder(CanvasDevice::GetSharedDevice());
	float extent = radius * extent_ratio;
	float alpha = float(flatan(radius, extent));
	float rad180 = degrees_to_radians(180.0);

	if (leftward) {
		base->BeginFigure(extent, radius);
		base->AddLine(-radius, radius);
		base->AddLine(-radius, 0.0F);
		base->AddArc(float2(0.0F, 0.0F), radius, radius, rad180, rad180 - alpha);
		base->AddLine(extent, radius);
	} else {
		base->BeginFigure(-extent, radius);
		base->AddLine(radius, radius);
		base->AddLine(radius, 0.0F);
		base->AddArc(float2(0.0F, 0.0F), radius, radius, 0.0F, alpha - rad180);
		base->AddLine(-extent, radius);
	}

	base->EndFigure(CanvasFigureLoop::Closed);

	return CanvasGeometry::CreatePath(base);
}

/*************************************************************************************************/
Gantrylet::Gantrylet(float radius, float extent, double degrees)
	: Gantrylet(GantryState::Default, radius, extent, degrees) {}

Gantrylet::Gantrylet(GantryState default_state, float radius, float extent, double degrees)
	: IStatelet(default_state), thickness(2.0F), leftward(radius < 0.0F), base_extent_ratio(extent), degrees(degrees) {
	float wo_x, wo_y;

	this->radius = flabs(radius);
	this->bottom_base_radius = this->radius * 0.085F;
	this->pivot_base_radius = this->thickness * 1.618F;
	this->pulley_radius = this->bottom_base_radius * 1.0F;
	this->hat_radiusX = this->pulley_radius * 1.2F;
	this->hat_radiusY = this->hat_radiusX * 0.618F;

	circle_point(this->radius, degrees + 90.0, &wo_x, &wo_y);
	
	this->width = this->bottom_base_radius * this->base_extent_ratio + flabs(wo_x) + this->pulley_radius * 2.0F + this->thickness;
	this->height = this->radius + this->bottom_base_radius + this->hat_radiusY + this->thickness;
}

void Gantrylet::construct() {
	this->pulley = circle(this->pulley_radius);
	this->pivot_base = make_pivot_base(this->pivot_base_radius, this->base_extent_ratio, this->leftward);
	this->bottom_base = make_pivot_base(this->bottom_base_radius, this->base_extent_ratio, this->leftward);

	this->make_hat(0.0);
	this->winding_style = make_dash_stroke(CanvasDashStyle::Dash);
}

void Gantrylet::update(long long count, long long interval, long long uptime) {
	if (this->winding) {
		switch (this->get_state()) {
		case GantryState::PullingIn: {
			this->winding_style->DashOffset = +float(count);
			this->notify_updated();
		}; break;
		case GantryState::PushingOut: {
			this->winding_style->DashOffset = -float(count);
			this->notify_updated();
		}; break;
		case GantryState::Default: {
			// do nothing
		}; break;
		}
	}
}

void Gantrylet::fill_extent(float x, float y, float* w, float* h) {
	SET_VALUES(w, this->width, h, this->height);
}

void Gantrylet::fill_margin(float x, float y, float* t, float* r, float* b, float* l) {
	GantryState s = this->get_state();
	
	SET_BOX(t, this->hat_y);
	SET_BOX(b, 0.0F);

	if (s == GantryState::PushedOut) {
		SET_BOXES(l, r, 0.0F);
	} else {
		float hspace = this->width
			- this->bottom_base_radius * this->base_extent_ratio
			- this->pulley_radius * 2.0F
			- this->thickness;

		SET_BOX(t, 0.0F);

		if (this->leftward) {
			SET_BOX(l, hspace);
			SET_BOX(r, 0.0F);
		} else {
			SET_BOX(l, 0.0F);
			SET_BOX(r, hspace);
		}
	}
}

void Gantrylet::prepare_style(GantryState status, GantryStyle& style) {
	CAS_SLOT(style.border_color, Colours::DarkGray);
	CAS_SLOT(style.winding_color, gantry_default_progress_color);
	CAS_SLOT(style.color, gantry_default_color);
	
	switch (status) {
	case GantryState::PushedOut: {
		CAS_SLOT(style.hat_color, Colours::Green);
	}; break;
	case GantryState::PulledIn: {
		CAS_SLOT(style.base_color, Colours::Green);
	}; break;
	case GantryState::PushingOut: case GantryState::PullingIn: {
		CAS_SLOT(style.pulley_color, style.winding_color);
	}; break;
	}

	CAS_SLOT(style.pulley_color, gantry_default_pulley_color);
	CAS_SLOT(style.hat_color, style.color);
	CAS_SLOT(style.base_color, style.color);
}

void Gantrylet::on_state_changed(GantryState status) {
	switch (status) {
	case GantryState::PushedOut: {
		this->make_hat(1.0);
		this->winding = false;
	}; break;
	case GantryState::PulledIn: {
		this->make_hat(0.0);
		this->winding = false;
	}; break;
	case GantryState::PushingOut: {
		this->make_hat(0.5);
		this->winding = true;
	}; break;
	case GantryState::PullingIn: {
		this->make_hat(0.5);
		this->winding = true;
	}; break;
	case GantryState::Default: {
		// keep current settings, but animation is paused by `update()`.
	}; break;
	}
}

float Gantrylet::get_winch_joint_y() {
	return this->height * 0.5F + this->pivot_base_radius * 2.0F;
}

void Gantrylet::draw(CanvasDrawingSession^ ds, float x, float y, float Width, float Height) {
	GantryStyle s = this->get_style();
	float thickoff = this->thickness * 0.5F;
	float mcx = x + this->pivot_base_radius * this->base_extent_ratio + thickoff;
	float mcy = y + this->get_winch_joint_y() - this->pivot_base_radius;
	float bcx = x + this->bottom_base_radius * this->base_extent_ratio + thickoff;
	float bcy = y + this->height - this->bottom_base_radius - thickoff;
	float hcx = bcx + this->hat_cxoff;
	float hcy = y + this->hat_cy;
	float pcx = hcx + this->pulley_radius;
	float pcy = hcy + this->pulley_radius + this->pulley_cyoff;
	float mtcx = hcx - this->pivot_base_radius;
	
	if (this->leftward) {
		mcx = x + this->width - (mcx - x);
		bcx = x + this->width - (bcx - x);
		pcx = x + this->width - (pcx - x);
		hcx = x + this->width - (hcx - x);
		mtcx = x + this->width - (mtcx - x);
	}

	ds->DrawGeometry(this->pulley, pcx, pcy, s.pulley_color, this->thickness);

	ds->DrawLine(mcx, mcy, mtcx, hcy, s.color, this->pivot_base_radius);
	
	if (this->winding) {
		ds->DrawLine(mcx, mcy, mtcx, hcy, s.winding_color, this->pivot_base_radius, this->winding_style);
	}

	ds->FillGeometry(this->pivot_base, mcx, mcy, s.color);
	ds->DrawGeometry(this->pivot_base, mcx, mcy, s.border_color, this->thickness);

	ds->DrawLine(bcx, bcy, hcx, hcy, s.color, this->bottom_base_radius);
	ds->FillGeometry(this->bottom_base, bcx, bcy, s.base_color);
	ds->DrawGeometry(this->bottom_base, bcx, bcy, s.border_color, this->thickness);

	ds->FillGeometry(this->hat, hcx, hcy, s.hat_color);
	ds->DrawGeometry(this->hat, hcx, hcy, s.border_color, this->thickness);
}

void Gantrylet::make_hat(double ratio) {
	auto hat = rounded_rectangle(-this->hat_radiusX, -this->hat_radiusY, this->hat_radiusX * 2.0F, this->hat_radiusY * 2.0F);
	double this_degrees = this->degrees * ratio;
	float wx, wy;

	circle_point(this->radius, this_degrees + 90.0, &wx, &wy);

	this->pulley_cyoff = this->hat_radiusX * flsin(degrees_to_radians(this_degrees));
	this->hat_cxoff = flabs(wx);
	this->hat_cy = this->height - this->bottom_base_radius - flabs(wy) - this->thickness * 0.5F;
	this->hat = geometry_rotate(hat, this_degrees * (this->leftward ? -1.0 : 1.0), 0.0F, 0.0F);
	this->hat_y = this->hat_cy - this->hat->ComputeBounds().Height * 0.5F;
}

/*************************************************************************************************/
GantrySymbollet::GantrySymbollet(float width, float height)
	: GantrySymbollet(GantryState::Default, width, height) {}

GantrySymbollet::GantrySymbollet(GantryState default_state, float width, float height)
	: IStatelet(default_state), width(flabs(width)), height(height), leftward(width < 0.0F), thickness(1.0F) {
	if (this->height <= 0.0F) {
		this->height = this->width * 0.618F;
	}
}

void GantrySymbollet::construct() {
	float halfwidth = this->width * 0.5F;
	float arrow_radiusX = halfwidth * 0.1618F;
	float arrow_radiusY = this->height * 0.5F;
	float xoff = halfwidth - arrow_radiusX - this->thickness * 0.5F;

	this->leftward_arrow = geometry_translate(polar_arrowhead(arrow_radiusX, arrow_radiusY, 180.0), -xoff, 0.F);
	this->rightward_arrow = geometry_translate(polar_arrowhead(arrow_radiusX, arrow_radiusY, 0.0), +xoff, 0.F);
}

void GantrySymbollet::update(long long count, long long interval, long long uptime) {
	switch (this->get_state()) {
	case GantryState::PullingIn: case GantryState::PushingOut: {
		this->highlighting = !this->highlighting;
		this->notify_updated();
	}; break;
	case GantryState::Default: {
		// do nothing
	}; break;
	}
}

void GantrySymbollet::fill_extent(float x, float y, float* w, float* h) {
	SET_VALUES(w, this->width, h, this->height);
}

void GantrySymbollet::prepare_style(GantryState status, GantrySymbolStyle& style) {
	CAS_SLOT(style.color, Colours::Gray);
	CAS_SLOT(style.highlight_color, Colours::Green);
}

void GantrySymbollet::on_state_changed(GantryState status) {
	GantrySymbolStyle s = this->get_style();

	switch (status) {
	case GantryState::PushedOut: {
		this->inside_color = s.color;
		this->outside_color = s.highlight_color;
		this->inside_border_color = nullptr;
		this->outside_border_color = nullptr;
		this->highlighting = true;
	}; break;
	case GantryState::PulledIn: {
		this->inside_color = s.highlight_color;
		this->outside_color = s.color;
		this->inside_border_color = nullptr;
		this->outside_border_color = nullptr;
		this->highlighting = true;
	}; break;
	case GantryState::PushingOut: {
		this->inside_color = s.color;
		this->outside_color = s.highlight_color;
		this->inside_border_color = nullptr;
		this->outside_border_color = s.highlight_color;
		this->highlighting = true;
	}; break;
	case GantryState::PullingIn: {
		this->inside_color = s.highlight_color;
		this->outside_color = s.color;
		this->inside_border_color = s.highlight_color;
		this->outside_border_color = nullptr;
		this->highlighting = true;
	}; break;
	case GantryState::Default: {
		this->highlighting = false;
	}; break;
	}
}

void GantrySymbollet::draw(CanvasDrawingSession^ ds, float x, float y, float Width, float Height) {
	GantrySymbolStyle s = this->get_style();
	float cx = x + this->width * 0.5F;
	float cy = y + this->height * 0.5F;

	if (!this->highlighting) {
		ds->FillGeometry(this->leftward_arrow, cx, cy, s.color);
		ds->FillGeometry(this->rightward_arrow, cx, cy, s.color);
	} else if (this->leftward) {
		ds->FillGeometry(this->leftward_arrow, cx, cy, this->outside_color);
		ds->FillGeometry(this->rightward_arrow, cx, cy, this->inside_color);
	} else {
		ds->FillGeometry(this->leftward_arrow, cx, cy, this->inside_color);
		ds->FillGeometry(this->rightward_arrow, cx, cy, this->outside_color);
	}

	if (this->leftward) {
		if (this->outside_border_color != nullptr) {
			ds->DrawGeometry(this->leftward_arrow, cx, cy, this->outside_border_color, this->thickness);
		}

		if (this->inside_border_color != nullptr) {
			ds->DrawGeometry(this->rightward_arrow, cx, cy, this->inside_border_color, this->thickness);
		}
	} else {
		if (this->inside_border_color != nullptr) {
			ds->DrawGeometry(this->leftward_arrow, cx, cy, this->inside_border_color, this->thickness);
		}

		if (this->outside_border_color != nullptr) {
			ds->DrawGeometry(this->rightward_arrow, cx, cy, this->outside_border_color, this->thickness);
		}
	}
}
