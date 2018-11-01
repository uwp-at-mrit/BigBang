#include "graphlet/device/gantrylet.hpp"

#include "math.hpp"
#include "text.hpp"
#include "shape.hpp"
#include "paint.hpp"
#include "geometry.hpp"
#include "colorspace.hpp"

#include "measure/vhatchmark.hpp"

using namespace WarGrey::SCADA;

using namespace Windows::Foundation;
using namespace Windows::Foundation::Numerics;

using namespace Microsoft::Graphics::Canvas;
using namespace Microsoft::Graphics::Canvas::Text;
using namespace Microsoft::Graphics::Canvas::Brushes;
using namespace Microsoft::Graphics::Canvas::Geometry;

static CanvasSolidColorBrush^ gantry_default_color = Colours::Yellow;
static CanvasSolidColorBrush^ gantry_default_pulley_color = Colours::DarkGray;
static CanvasSolidColorBrush^ gantry_default_progress_color = Colours::Gray;

static CanvasGeometry^ make_pivot_base(float radius, float extent_ratio, bool leftward) {
	auto base = ref new CanvasPathBuilder(CanvasDevice::GetSharedDevice());
	float extent = radius * extent_ratio;
	float alpha = std::atan2(radius, extent);
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
Gantrylet::Gantrylet(float radius, double degrees)
	: Gantrylet(GantryStatus::WindedUp, radius, degrees) {}

Gantrylet::Gantrylet(GantryStatus default_status, float radius, double degrees)
	: IStatuslet(default_status), thickness(2.0F), leftward(radius < 0.0F), degrees(degrees) {
	float r = std::fabsf(radius);
	float wo_x, wo_y;

	this->bottom_base_radius = r * 0.0618F;
	this->pivot_base_radius = this->thickness * 1.618F;
	this->pulley_radius = this->bottom_base_radius * 1.0F;
	this->top_radiusX = this->pulley_radius * 1.2F;
	this->top_radiusY = this->top_radiusX * 0.618F;
	this->base_extent_ratio = 3.0F;

	circle_point(r, degrees + 90.0, &wo_x, &wo_y);
	
	this->width = this->bottom_base_radius * this->base_extent_ratio + std::fabsf(wo_x) + this->pulley_radius + this->thickness;
	this->height = r + this->bottom_base_radius + this->top_radiusY + this->thickness;
	this->winded_top_cy = this->height - this->bottom_base_radius - std::fabsf(wo_y) - this->thickness;
}

void Gantrylet::construct() {
	this->pulley = circle(this->pulley_radius);
	this->top = rounded_rectangle(-this->top_radiusX, -this->top_radiusY, this->top_radiusX * 2.0F, this->top_radiusY * 2.0F);
	this->winded_top = geometry_rotate(this->top, this->degrees * (this->leftward ? -1.0 : 1.0), 0.0F, 0.0F);
	this->pivot_base = make_pivot_base(this->pivot_base_radius, this->base_extent_ratio, this->leftward);
	this->bottom_base = make_pivot_base(this->bottom_base_radius, this->base_extent_ratio, this->leftward);

	this->winded_top_y = this->winded_top_cy - this->winded_top->ComputeBounds().Height * 0.5F;
}

void Gantrylet::fill_extent(float x, float y, float* w, float* h) {
	SET_VALUES(w, this->width, h, this->height);
}

void Gantrylet::fill_margin(float x, float y, float* t, float* r, float* b, float* l) {
	SET_BOX(b, 0.0F);

	if (this->winded_out) {
		SET_BOX(t, this->winded_top_y);
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

void Gantrylet::prepare_style(GantryStatus status, GantryStyle& style) {
	CAS_SLOT(style.border_color, Colours::DarkGray);
	CAS_SLOT(style.pulley_color, Colours::DimGray);
	CAS_SLOT(style.color, Colours::Yellow);
}

void Gantrylet::on_status_changed(GantryStatus status) {
	switch (status) {
	case GantryStatus::WindedOut: {
		this->winded_out = true;
	}; break;
	default: {
		this->winded_out = false;
	}
	}
}

void Gantrylet::draw(CanvasDrawingSession^ ds, float x, float y, float Width, float Height) {
	GantryStyle s = this->get_style();
	auto top = (this->winded_out ? this->winded_top : this->top);
	float thickoff = this->thickness * 0.5F;
	float mcx = x + this->pivot_base_radius * this->base_extent_ratio + thickoff;
	float mcy = y + this->height * 0.5F + this->pivot_base_radius;
	float bcx = x + this->bottom_base_radius * this->base_extent_ratio + thickoff;
	float bcy = y + this->height - this->bottom_base_radius - thickoff;
	float pcx = (this->winded_out ? (x + this->width - this->pulley_radius - thickoff) : (bcx + this->pulley_radius));
	float tcx = pcx - this->pulley_radius;
	float tcy = y + (this->winded_out ? this->winded_top_cy : (this->top_radiusY + thickoff));
	float pcy = tcy + this->pulley_radius * (this->winded_out ? 1.618F : 1.0F);
	
	if (this->leftward) {
		mcx = x + this->width - (mcx - x);
		bcx = x + this->width - (bcx - x);
		pcx = x + this->width - (pcx - x);
		tcx = x + this->width - (tcx - x);
	}

	{ // draw boarder
		float ts, rs, bs, ls;

		this->fill_margin(x, y, &ts, &rs, &bs, &ls);

		ds->DrawRectangle(x, y, this->width, this->height, Colours::Crimson);
		ds->DrawRectangle(x + ls, y + ts, this->width - ls - rs, this->height - ts - bs, Colours::RoyalBlue);
	}

	ds->DrawGeometry(this->pulley, pcx, pcy, s.pulley_color, this->thickness);

	ds->DrawLine(bcx, bcy, tcx, tcy, s.color, this->bottom_base_radius);
	ds->DrawLine(mcx, mcy, tcx, tcy, s.color, this->pivot_base_radius);

	ds->FillGeometry(top, tcx, tcy, s.color);
	ds->DrawGeometry(top, tcx, tcy, s.border_color, this->thickness);

	ds->FillGeometry(this->pivot_base, mcx, mcy, s.color);
	ds->DrawGeometry(this->pivot_base, mcx, mcy, s.border_color, this->thickness);

	ds->FillGeometry(this->bottom_base, bcx, bcy, s.color);
	ds->DrawGeometry(this->bottom_base, bcx, bcy, s.border_color, this->thickness);
}
