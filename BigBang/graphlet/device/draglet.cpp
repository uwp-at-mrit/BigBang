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

static CanvasSolidColorBrush^ drag_default_color = Colours::DimGray;
static CanvasSolidColorBrush^ drag_default_head_color = Colours::DimGray;
static CanvasSolidColorBrush^ drag_default_body_color = Colours::Yellow;
static CanvasSolidColorBrush^ drag_default_hatchmark_color = Colours::GhostWhite;

static inline float drag_pt(float height, double depth, double highest, double lowest, float yoff) {
	float percentage = float((highest - depth) / (highest - lowest));
	
	return height * percentage + yoff;
}

/*************************************************************************************************/
IDraglet::IDraglet(DragInfo& info, float width, float height, float thickness
	, ICanvasBrush^ color, ICanvasBrush^ hcolor, ICanvasBrush^ bcolor, ICanvasBrush^ hmcolor)
	: info(info), width(std::fabsf(width)), height(height), thickness(thickness), leftward(width < 0.0F)
	, head_color(hcolor == nullptr ? drag_default_head_color : hcolor)
	, body_color(bcolor == nullptr ? drag_default_body_color : bcolor)
	, hatchmark_color(hmcolor == nullptr ? drag_default_hatchmark_color : hmcolor) {

	unsigned int count = sizeof(this->info.pipe_lengths) / sizeof(float);

	this->drag_thickness = this->thickness * 3.14F;
	this->length = this->info.trunnion_length + this->info.head_length + this->info.pipe_padding;
	for (unsigned int idx = 0; idx < count; idx++) {
		this->length += this->info.pipe_lengths[idx];
	}
}

void IDraglet::fill_extent(float x, float y, float* w, float* h) {
	SET_VALUES(w, this->width, h, this->height);
}

void IDraglet::set_position(float3& trunnion, float3 intermediates[], float3& draghead, bool force) {
	unsigned int count = sizeof(this->info.pipe_lengths) / sizeof(float);
	bool tchanged = (!this->position_equal(this->trunnion, trunnion));
	bool dchanged = (!this->position_equal(this->draghead, draghead));
	bool ichanged = false;

	for (unsigned int idx = 0; idx < count; idx++) {
		bool changed = (!this->position_equal(this->intermediates[idx], intermediates[idx]));

		if (changed) {
			this->intermediates[idx] = intermediates[idx];
			ichanged = true;
		}
	}

	if (force || tchanged || ichanged || dchanged) {
		if (tchanged) {
			this->trunnion = trunnion;
		}

		if (dchanged) {
			this->draghead = draghead;
		}

		this->on_position_changed(this->trunnion, this->intermediates, this->draghead);
	}
}

/*************************************************************************************************/
DragXZlet::DragXZlet(DragInfo& info, float width, float height, unsigned int color
	, float thickness, float interval, ICanvasBrush^ hcolor, ICanvasBrush^ bcolor, ICanvasBrush^ hmcolor)
	: IDraglet(info, width, height, thickness, Colours::make(color), hcolor, bcolor, hmcolor)
	, depth_highest(interval) {
	this->depth_lowest = -50.0F;
	this->suction_lowest = -20.0F;
}

void DragXZlet::construct() {
	VHatchMarkMetrics hmetrics, tmetrics;
	float head_range = float(this->depth_highest - this->depth_lowest);
	float tail_range = float(this->depth_highest - this->suction_lowest);
	float head_height = this->height;
	float tail_height = head_height * tail_range / head_range;
	unsigned int head_step = ((unsigned int)std::roundf(head_range)) / 5U;
	unsigned int tail_step = ((unsigned int)std::roundf(tail_range)) / 5U;
	float epr = this->drag_thickness * 1.618F * 0.5F;

	if (this->leftward) {
		auto head = vlhatchmark(head_height, this->depth_lowest, this->depth_highest, head_step, this->thickness, &hmetrics);
		auto tail = vrhatchmark(tail_height, this->suction_lowest, this->depth_highest, tail_step, this->thickness, &tmetrics);

		this->hatchmarks = geometry_freeze(geometry_union(head, tail, this->width - tmetrics.width, 0.0F));
		this->ws_x = this->width - tmetrics.width - tmetrics.em - epr;
		this->ws_width = (hmetrics.width + hmetrics.em + epr) - this->ws_x;
	} else {
		auto head = vrhatchmark(head_height, this->depth_lowest, this->depth_highest, head_step, this->thickness, &hmetrics);
		auto tail = vlhatchmark(tail_height, this->suction_lowest, this->depth_highest, tail_step, this->thickness, &tmetrics);

		this->hatchmarks = geometry_freeze(geometry_union(head, this->width - hmetrics.width, 0.0F, tail));
		this->ws_x = tmetrics.width + tmetrics.em + epr;
		this->ws_width = (this->width - hmetrics.width - hmetrics.em - epr) - this->ws_x;
	}

	{ // make drag
		this->ws_y = hmetrics.hatch_y;
		this->ws_height = hmetrics.hatch_height;

		this->pivot = circle(epr);
		this->suction_style = make_dash_stroke(CanvasDashStyle::Dash);

		this->set_position(this->trunnion, this->intermediates, this->draghead, true);
	}
}

void DragXZlet::on_position_changed(float3& trunnion, float3 intermediates[], float3& draghead) {
	unsigned int count = sizeof(this->intermediate_pinxs) / sizeof(float);
	
	this->trunnion_pinx = drag_pt(this->ws_width, trunnion.x, this->length, 0.0F, this->ws_x);
	this->trunnion_piny = drag_pt(this->ws_height, -trunnion.z, this->depth_highest, this->depth_lowest, this->ws_y);
	this->draghead_pinx = drag_pt(this->ws_width, draghead.x, this->length, 0.0F, this->ws_x);
	this->draghead_piny = drag_pt(this->ws_height, -draghead.z, this->depth_highest, this->depth_lowest, this->ws_y);

	for (unsigned int idx = 0; idx < count; idx++) {
		this->intermediate_pinxs[idx] = drag_pt(this->ws_width, intermediates[idx].x, this->length, 0.0F, this->ws_x);
		this->intermediate_pinys[idx] = drag_pt(this->ws_height, -intermediates[idx].z, this->depth_highest, this->depth_lowest, this->ws_y);
	}
}

void DragXZlet::draw(CanvasDrawingSession^ ds, float x, float y, float Width, float Height) {
	float dx = x + this->draghead_pinx;
	float dy = y + this->draghead_piny;
	float ix = x + this->intermediate_pinxs[0];
	float iy = y + this->intermediate_pinys[0];
	float tx = x + this->trunnion_pinx;
	float ty = y + this->trunnion_piny;

	ds->DrawCachedGeometry(this->hatchmarks, x, y, this->hatchmark_color);

	ds->DrawLine(dx, dy, ix, iy, this->body_color, this->drag_thickness);
	ds->DrawLine(dx, dy, ix, iy, this->color, 1.0F, this->suction_style);
	ds->DrawLine(ix, iy, tx, ty, this->body_color, this->drag_thickness);
	ds->DrawLine(ix, iy, tx, ty, this->color, 1.0F, this->suction_style);

	ds->FillGeometry(this->pivot, dx, dy, this->color);
	ds->DrawGeometry(this->pivot, dx, dy, this->body_color, this->thickness);
	ds->FillGeometry(this->pivot, ix, iy, this->color);
	ds->DrawGeometry(this->pivot, ix, iy, this->body_color, this->thickness);
	ds->FillGeometry(this->pivot, tx, ty, this->color);
	ds->DrawGeometry(this->pivot, tx, ty, this->body_color, this->thickness);

	ds->DrawRectangle(x, y, this->width, this->height, Colours::DodgerBlue);
}

bool DragXZlet::position_equal(float3& old_pos, float3& new_pos) {
	return ((old_pos.x == new_pos.x) && (old_pos.z == new_pos.z));
}

/*************************************************************************************************/

