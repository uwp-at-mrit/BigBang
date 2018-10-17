#include "graphlet/device/draglet.hpp"

#include "math.hpp"
#include "shape.hpp"
#include "paint.hpp"
#include "geometry.hpp"

#include "tongue.hpp"
#include "string.hpp"

#include "measure/vhatchmark.hpp"

using namespace WarGrey::SCADA;

using namespace Windows::Foundation;
using namespace Windows::Foundation::Numerics;

using namespace Microsoft::Graphics::Canvas;
using namespace Microsoft::Graphics::Canvas::Brushes;
using namespace Microsoft::Graphics::Canvas::Geometry;

static CanvasSolidColorBrush^ drag_default_head_color = Colours::DimGray;
static CanvasSolidColorBrush^ drag_default_body_color = Colours::Yellow;
static CanvasSolidColorBrush^ drag_default_hatchmark_color = Colours::GhostWhite;

static inline float drag_y(float height, double depth, double highest, double lowest, float yoff) {
	float percentage = float((highest - depth) / (highest - lowest));
	
	return height * percentage + yoff;
}

/*************************************************************************************************/
Draglet::Draglet(float width, float height, unsigned int visor_color, float thickness
	, ICanvasBrush^ hcolor, ICanvasBrush^ bcolor, ICanvasBrush^ hmcolor)
	: Draglet(5.0, -50.0, -20.0, width, height, visor_color, thickness, hcolor, bcolor, hmcolor) {}

Draglet::Draglet(double dmax, double dmin, double tmin, float width, float height, unsigned int visor_color, float thickness
	, ICanvasBrush^ hcolor, ICanvasBrush^ bcolor, ICanvasBrush^ hmcolor)
	: depth_highest(dmax), depth_lowest(dmin), trunnion_lowest(tmin), width(std::fabsf(width)), height(height)
	, thickness(thickness), leftward(width < 0.0F), visor_color(Colours::make(visor_color))
	, head_color(hcolor == nullptr ? drag_default_head_color : hcolor)
	, body_color(bcolor == nullptr ? drag_default_body_color : bcolor)
	, hatchmark_color(hmcolor == nullptr ? drag_default_hatchmark_color : hmcolor) {}

void Draglet::construct() {
	VHatchMarkMetrics hmetrics, tmetrics;
	float head_range = float(this->depth_highest - this->depth_lowest);
	float tail_range = float(this->depth_highest - this->trunnion_lowest);
	float head_height = this->height;
	float tail_height = head_height * tail_range / head_range;
	unsigned int head_step = ((unsigned int)std::roundf(head_range)) / 5U;
	unsigned int tail_step = ((unsigned int)std::roundf(tail_range)) / 5U;
	float epr = this->thickness * 2.4F;
	
	if (this->leftward) {
		auto head = vlhatchmark(head_height, this->depth_lowest, this->depth_highest, head_step, this->thickness, &hmetrics);
		auto tail = vrhatchmark(tail_height, this->trunnion_lowest, this->depth_highest, tail_step, this->thickness, &tmetrics);

		this->hatchmarks = geometry_freeze(geometry_union(head, tail, this->width - tmetrics.width, 0.0F));
		this->draghead_pinx = hmetrics.width + hmetrics.em + epr;
		this->trunnion_pinx = this->width - tmetrics.width - tmetrics.em - epr;
	} else {
		auto head = vrhatchmark(head_height, this->depth_lowest, this->depth_highest, head_step, this->thickness, &hmetrics);
		auto tail = vlhatchmark(tail_height, this->trunnion_lowest, this->depth_highest, tail_step, this->thickness, &tmetrics);

		this->hatchmarks = geometry_freeze(geometry_union(head, this->width - hmetrics.width, 0.0F, tail));
		this->draghead_pinx = this->width - hmetrics.width - hmetrics.em - epr;
		this->trunnion_pinx = tmetrics.width + tmetrics.em + epr;
	}

	{ // make drag
		float drag_length = this->trunnion_pinx - this->draghead_pinx;

		this->ws_top = hmetrics.hatch_y;
		this->ws_height = hmetrics.hatch_height;
		this->drag_thickness = epr * 2.0F * 0.618F;
		this->trunnion_length = drag_length * 0.382F;
		this->intermediate_length = drag_length - this->trunnion_length;
		
		this->pivot = circle(epr);
		this->suction_style = make_dash_stroke(CanvasDashStyle::Dash);

		this->set_depths(-24.0, -8.0, 0.0, true);
	}
}

void Draglet::fill_extent(float x, float y, float* w, float* h) {
	SET_VALUES(w, this->width, h, this->height);
}

void Draglet::set_depths(double draghead, double intermediate, double trunnion, bool force) {
	if (force || (this->draghead_depth != draghead) || (this->intermediate_depth != intermediate) || (this->trunnion_depth != trunnion)) {
		this->draghead_depth = draghead;
		this->intermediate_depth = intermediate;
		this->trunnion_depth = trunnion;

		this->trunnion_piny = drag_y(this->ws_height, trunnion, this->depth_highest, this->depth_lowest, this->ws_top);
		this->intermediate_pinx = this->trunnion_pinx - this->trunnion_length;
		this->intermediate_piny = drag_y(this->ws_height, intermediate, this->depth_highest, this->depth_lowest, this->ws_top);
		this->draghead_pinx = this->intermediate_pinx - this->intermediate_length;
		this->draghead_piny = drag_y(this->ws_height, draghead, this->depth_highest, this->depth_lowest, this->ws_top);
	}
}

void Draglet::draw(CanvasDrawingSession^ ds, float x, float y, float Width, float Height) {
	float dx = x + this->draghead_pinx;
	float dy = y + this->draghead_piny;
	float ix = x + this->intermediate_pinx;
	float iy = y + this->intermediate_piny;
	float tx = x + this->trunnion_pinx;
	float ty = y + this->trunnion_piny;

	ds->DrawCachedGeometry(this->hatchmarks, x, y, this->hatchmark_color);

	ds->DrawLine(dx, dy, ix, iy, this->body_color, this->drag_thickness);
	ds->DrawLine(dx, dy, ix, iy, this->visor_color, 1.0F, this->suction_style);
	ds->DrawLine(ix, iy, tx, ty, this->body_color, this->drag_thickness);
	ds->DrawLine(ix, iy, tx, ty, this->visor_color, 1.0F, this->suction_style);
	ds->DrawLine(x, dy, x + this->width, dy, this->visor_color, 1.0F);
	ds->DrawLine(x, iy, x + this->width, iy, this->visor_color, 1.0F);

	ds->FillGeometry(this->pivot, dx, dy, this->visor_color);
	ds->DrawGeometry(this->pivot, dx, dy, this->body_color, this->thickness);
	ds->FillGeometry(this->pivot, tx, ty, this->visor_color);
	ds->DrawGeometry(this->pivot, tx, ty, this->body_color, this->thickness);

	ds->DrawRectangle(x, y, this->width, this->height, Colours::DodgerBlue);
}
