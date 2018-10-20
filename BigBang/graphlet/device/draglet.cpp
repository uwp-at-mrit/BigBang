#include "graphlet/device/draglet.hpp"

#include "math.hpp"
#include "shape.hpp"
#include "paint.hpp"
#include "geometry.hpp"

#include "string.hpp"

#include "measure/vhatchmark.hpp"
#include "measure/hhatchmark.hpp"

using namespace WarGrey::SCADA;

using namespace Windows::Foundation;
using namespace Windows::Foundation::Numerics;

using namespace Microsoft::Graphics::Canvas;
using namespace Microsoft::Graphics::Canvas::Text;
using namespace Microsoft::Graphics::Canvas::Brushes;
using namespace Microsoft::Graphics::Canvas::Geometry;

static CanvasSolidColorBrush^ drag_default_color = Colours::DimGray;
static CanvasSolidColorBrush^ drag_default_meter_color = Colours::GhostWhite;
static CanvasSolidColorBrush^ drag_default_head_color = Colours::DimGray;
static CanvasSolidColorBrush^ drag_default_body_color = Colours::Yellow;
static CanvasSolidColorBrush^ drag_default_hatchmark_color = Colours::Silver;

static inline float drag_pt(float height, double depth, double highest, double lowest, float yoff) {
	float percentage = float((highest - depth) / (highest - lowest));
	
	return height * percentage + yoff;
}

/*************************************************************************************************/
IDraglet::IDraglet(DragInfo& info, float width, float height, float thickness
	, ICanvasBrush^ color, ICanvasBrush^ mcolor, ICanvasBrush^ hcolor, ICanvasBrush^ bcolor, ICanvasBrush^ hmcolor
	, CanvasTextFormat^ font)
	: info(info), width(std::fabsf(width)), height(height), thickness(thickness), precision(2U)
	, leftward(width < 0.0F), mfont(font), color(color == nullptr ? drag_default_color : color)
	, meter_color(mcolor == nullptr ? drag_default_meter_color : mcolor)
	, head_color(hcolor == nullptr ? drag_default_head_color : hcolor)
	, body_color(bcolor == nullptr ? drag_default_body_color : bcolor)
	, hatchmark_color(hmcolor == nullptr ? drag_default_hatchmark_color : hmcolor) {

	this->total_length = this->info.trunnion_length + this->info.head_length + this->info.pipe_padding;
	for (unsigned int idx = 0; idx < sizeof(this->info.pipe_lengths) / sizeof(float); idx++) {
		this->total_length += this->info.pipe_lengths[idx];
	}

	this->drag_thickness = this->thickness * 3.14F;
	this->suction_style = make_dash_stroke(CanvasDashStyle::Dash);

	if (this->mfont == nullptr) {
		this->mfont = make_text_format(20.0F);
	}
}

void IDraglet::fill_extent(float x, float y, float* w, float* h) {
	SET_VALUES(w, this->width, h, this->height);
}

void IDraglet::set_position(float suction_depth, float3 ujoints[], float3& draghead, bool force) {
	bool schanged = (this->suction_depth != suction_depth);
	bool dchanged = (!this->position_equal(this->draghead, draghead));
	bool ichanged = false;

	for (unsigned int idx = 0; idx < DRAG_SEGMENT_MAX_COUNT; idx++) {
		if (this->info.pipe_lengths[idx] > 0.0) {
			bool changed = (!this->position_equal(this->ujoints[idx], ujoints[idx]));

			if (changed) {
				this->ujoints[idx] = ujoints[idx];
				ichanged = true;
			}
		}
	}

	if (force || schanged || ichanged || dchanged) {
		if (dchanged) {
			this->draghead = draghead;
		}

		this->suction_depth = suction_depth;
		this->on_position_changed(this->suction_depth, this->ujoints, this->draghead);
	}
}

void IDraglet::draw_pipe_segment(CanvasDrawingSession^ ds, float ex, float ey, float sx, float sy, bool draw_joint) {
	ds->DrawLine(sx, sy, ex, ey, this->body_color, this->drag_thickness);
	ds->DrawLine(sx, sy, ex, ey, this->color, 1.0F, this->suction_style);

	if (draw_joint) {
		ds->FillGeometry(this->universal_joint, ex, ey, this->color);
		ds->DrawGeometry(this->universal_joint, ex, ey, this->body_color, this->thickness);
	}
}

/*************************************************************************************************/
DragXYlet::DragXYlet(DragInfo& info, float width, float height, unsigned int color, float interval, unsigned int ostep, unsigned int istep
	, float thickness, ICanvasBrush^ mcolor, ICanvasBrush^ hcolor, ICanvasBrush^ bcolor, ICanvasBrush^ hmcolor, CanvasTextFormat^ font)
	: IDraglet(info, width, height, thickness, Colours::make(color), mcolor, hcolor, bcolor, hmcolor, font) {
	this->outside_most = interval * float(ostep);
	this->inside_most = -interval * float(istep);
	this->step = ostep + istep;
}

void DragXYlet::construct() {
	HHatchMarkMetrics metrics;
	CanvasGeometry^ hm;
	float hm_width = this->width;
	
	if (this->leftward) {
		hm = hbhatchmark(hm_width, this->outside_most, this->inside_most, this->step, this->thickness, &metrics);

		this->ws_x = metrics.hatch_x;
		this->ws_width = metrics.hatch_width;
	} else {
		hm = hbhatchmark(hm_width, this->inside_most, this->outside_most, this->step, this->thickness, &metrics);

		this->ws_x = this->width - metrics.hatch_right_space;
		this->ws_width = -metrics.hatch_width;
	}

	{ // make axes
		float axis_y = this->height - metrics.height;
		
		this->suction_x = drag_pt(this->ws_width, 0.0, this->outside_most, this->inside_most, this->ws_x);
		this->hatchmarks = geometry_freeze(geometry_union(hm, 0.0F, axis_y, vline(axis_y, this->thickness), this->suction_x, 0.0F));
	}
	
	{ // make drag
		float epr = this->drag_thickness * 1.618F * 0.5F;
		this->ws_y = metrics.height * 2.0F;
		this->ws_height = this->height - this->ws_y * 2.0F;
		this->trunnion_x = drag_pt(this->ws_width, this->info.trunnion_gapsize, this->outside_most, this->inside_most, this->ws_x);
		
		this->universal_joint = circle(epr);

		this->set_position(0.0F, this->ujoints, this->draghead, true);
	}
}

void DragXYlet::on_position_changed(float suction_depth, float3 ujoints[], float3& draghead) {
	this->draghead_joint_x = drag_pt(this->ws_height, draghead.x, this->total_length, 0.0F, this->ws_y);
	this->draghead_joint_y = drag_pt(this->ws_width, draghead.y, this->outside_most, this->inside_most, this->ws_x);
	this->draghead_m = make_text_layout(flstring(draghead.y, this->precision), this->mfont);

	for (unsigned int idx = 0; idx < DRAG_SEGMENT_MAX_COUNT; idx++) {
		if (this->info.pipe_lengths[idx] > 0.0F) {
			this->ujoints_xs[idx] = drag_pt(this->ws_height, ujoints[idx].x, this->total_length, 0.0F, this->ws_y);
			this->ujoints_ys[idx] = drag_pt(this->ws_width, ujoints[idx].y, this->outside_most, this->inside_most, this->ws_x);
			this->ujoints_ms[idx] = make_text_layout(flstring(ujoints[idx].y, this->precision), this->mfont);
		}
	}
}

void DragXYlet::draw(CanvasDrawingSession^ ds, float x, float y, float Width, float Height) {
	float suction_x = x + this->suction_x;
	float trunnion_x = x + this->trunnion_x;
	float trunnion_y = y + this->ws_y;
	float draghead_x = x + this->draghead_joint_x;
	float draghead_y = y + this->draghead_joint_y;
	float last_joint_x = x + this->ujoints_xs[0];
	float last_joint_y = y + this->ujoints_ys[0];

	ds->DrawCachedGeometry(this->hatchmarks, x, y, this->hatchmark_color);
	this->draw_pipe_segment(ds, suction_x, trunnion_y, trunnion_x, trunnion_y, false);
	this->draw_pipe_segment(ds, trunnion_x, trunnion_y, last_joint_x, last_joint_y, false);

	{ // draw drag
		for (unsigned int idx = 1; idx < DRAG_SEGMENT_MAX_COUNT; idx++) {
			if (this->info.pipe_lengths[idx] > 0.0F) {
				float ix = x + this->ujoints_xs[idx];
				float iy = y + this->ujoints_ys[idx];

				this->draw_pipe_segment(ds, last_joint_x, last_joint_y, ix, iy);
				last_joint_x = ix;
				last_joint_y = iy;
			}
		}

		this->draw_pipe_segment(ds, last_joint_x, last_joint_y, draghead_x, draghead_y);

		{ // draw draghead
			ds->FillGeometry(this->universal_joint, draghead_x, draghead_y, this->color);
			ds->DrawGeometry(this->universal_joint, draghead_x, draghead_y, this->body_color, this->thickness);
		}
	}

	{ // draw meters
		for (unsigned int idx = 0; idx < DRAG_SEGMENT_MAX_COUNT; idx++) {
			if (this->info.pipe_lengths[idx] > 0.0F) {
				float ix = x + this->ujoints_xs[idx];
				float iy = y + this->ujoints_ys[idx];

				this->draw_meter(ds, this->ujoints_ms[idx], ix, iy, x);
			}
		}

		this->draw_meter(ds, this->draghead_m, draghead_x, draghead_y, x);
	}
}

void DragXYlet::draw_meter(CanvasDrawingSession^ ds, CanvasTextLayout^ meter, float joint_x, float joint_y, float gx) {
	if (meter != nullptr) {
		Rect box = meter->LayoutBounds;
		float x = joint_x - box.Width - this->drag_thickness;
		float y = joint_y - box.Height * 0.5F;

		if ((x < gx) || (x < this->suction_x + this->thickness)) {
			x = joint_x + this->drag_thickness;
		}
		
		if ((x + box.Width > gx + this->width) || (x > this->suction_x - this->thickness)) {
			x = joint_x - box.Width;
		}

		ds->DrawTextLayout(meter, x, y, this->meter_color);
	}
}

bool DragXYlet::position_equal(float3& old_pos, float3& new_pos) {
	return ((old_pos.x == new_pos.x) && (old_pos.y == new_pos.y));
}

/*************************************************************************************************/
DragXZlet::DragXZlet(DragInfo& info, float width, float height, unsigned int color, float interval, float suction_lowest
	, float thickness, ICanvasBrush^ mcolor, ICanvasBrush^ hcolor, ICanvasBrush^ bcolor, ICanvasBrush^ hmcolor, CanvasTextFormat^ font)
	: IDraglet(info, width, height, thickness, Colours::make(color), mcolor, hcolor, bcolor, hmcolor, font)
	, depth_highest(interval), suction_lowest(suction_lowest) {
	this->depth_lowest = -std::ceilf(this->total_length / interval) * interval;
}

void DragXZlet::construct() {
	VHatchMarkMetrics hmetrics, tmetrics;
	double head_range = this->depth_highest - this->depth_lowest;
	double tail_range = this->depth_highest - this->suction_lowest;
	float head_height = this->height;
	float tail_height = head_height * float(tail_range / head_range) + this->thickness * float(head_range / tail_range);
	unsigned int head_step = ((unsigned int)std::round(head_range / this->depth_highest));
	unsigned int tail_step = ((unsigned int)std::round(tail_range / this->depth_highest));
	float epr = this->drag_thickness * 1.618F * 0.5F;

	if (this->leftward) {
		auto head = vlhatchmark(head_height, this->depth_lowest, this->depth_highest, head_step, this->thickness, &hmetrics);
		auto tail = vrhatchmark(tail_height, this->suction_lowest, this->depth_highest, tail_step, this->thickness, &tmetrics);

		this->hatchmarks = geometry_freeze(geometry_union(head, tail, this->width - tmetrics.width, 0.0F));
		
		this->left_margin = hmetrics.width;
		this->right_margin = this->width - tmetrics.width;
		this->ws_x = this->right_margin - tmetrics.em - epr;
		this->ws_width = (this->left_margin + hmetrics.em + epr) - this->ws_x;
	} else {
		auto head = vrhatchmark(head_height, this->depth_lowest, this->depth_highest, head_step, this->thickness, &hmetrics);
		auto tail = vlhatchmark(tail_height, this->suction_lowest, this->depth_highest, tail_step, this->thickness, &tmetrics);

		this->hatchmarks = geometry_freeze(geometry_union(head, this->width - hmetrics.width, 0.0F, tail));
		
		this->left_margin = tmetrics.width;
		this->right_margin = this->width - hmetrics.width;
		this->ws_x = this->left_margin + tmetrics.em + epr;
		this->ws_width = (this->right_margin - hmetrics.em - epr) - this->ws_x;
	}

	{ // make drag
		this->ws_y = hmetrics.hatch_y;
		this->ws_height = hmetrics.hatch_height;

		this->universal_joint = circle(epr);

		this->set_position(0.0F, this->ujoints, this->draghead, true);
	}
}

void DragXZlet::on_position_changed(float suction_depth, float3 ujoints[], float3& draghead) {
	this->suction_y = drag_pt(this->ws_height, -suction_depth, this->depth_highest, this->depth_lowest, this->ws_y);
	this->suction_m = make_text_layout(flstring(suction_depth, this->precision), this->mfont);

	this->draghead_joint_x = drag_pt(this->ws_width, draghead.x, this->total_length, 0.0F, this->ws_x);
	this->draghead_joint_y = drag_pt(this->ws_height, -draghead.z, this->depth_highest, this->depth_lowest, this->ws_y);
	this->draghead_m = make_text_layout(flstring(draghead.z, this->precision), this->mfont);

	for (unsigned int idx = 0; idx < DRAG_SEGMENT_MAX_COUNT; idx++) {
		if (this->info.pipe_lengths[idx] > 0.0F) {
			this->ujoints_xs[idx] = drag_pt(this->ws_width, ujoints[idx].x, this->total_length, 0.0F, this->ws_x);
			this->ujoints_ys[idx] = drag_pt(this->ws_height, -ujoints[idx].z, this->depth_highest, this->depth_lowest, this->ws_y);
			this->ujoints_ms[idx] = make_text_layout(flstring(ujoints[idx].z, this->precision), this->mfont);
		}
	}
}

void DragXZlet::draw(CanvasDrawingSession^ ds, float x, float y, float Width, float Height) {
	float suction_x = x + this->ws_x;
	float suction_y = y + this->suction_y;
	float draghead_x = x + this->draghead_joint_x;
	float draghead_y = y + this->draghead_joint_y;
	float last_joint_x = suction_x;
	float last_joint_y = suction_y;
	
	ds->DrawCachedGeometry(this->hatchmarks, x, y, this->hatchmark_color);

	{ // draw drag
		for (unsigned int idx = 0; idx < DRAG_SEGMENT_MAX_COUNT; idx++) {
			if (this->info.pipe_lengths[idx] > 0.0F) {
				float ix = x + this->ujoints_xs[idx];
				float iy = y + this->ujoints_ys[idx];

				if (ix != suction_x) {
					this->draw_pipe_segment(ds, last_joint_x, last_joint_y, ix, iy);
					last_joint_x = ix;
					last_joint_y = iy;
				}
			}
		}

		this->draw_pipe_segment(ds, last_joint_x, last_joint_y, draghead_x, draghead_y);

		{ // draw draghead
			ds->FillGeometry(this->universal_joint, draghead_x, draghead_y, this->color);
			ds->DrawGeometry(this->universal_joint, draghead_x, draghead_y, this->body_color, this->thickness);
		}
	}
	
	{ // draw meters
		float lX = x + this->left_margin + this->thickness;
		float rX = x + this->right_margin - this->thickness;
		float Y = y + this->height;
	
		for (unsigned int idx = 0; idx < DRAG_SEGMENT_MAX_COUNT; idx++) {
			if (this->info.pipe_lengths[idx] > 0.0F) {
				float ix = x + this->ujoints_xs[idx];
				float iy = y + this->ujoints_ys[idx];

				if (ix != suction_x) {
					this->draw_meter(ds, this->ujoints_ms[idx], ix, iy, lX, rX, Y);
				}
			}
		}

		this->draw_meter(ds, this->suction_m, suction_x, suction_y, lX, rX, Y);
		this->draw_meter(ds, this->draghead_m, draghead_x, draghead_y, lX, rX, Y);
	}
}

void DragXZlet::draw_meter(CanvasDrawingSession^ ds, CanvasTextLayout^ meter, float joint_x, float joint_y, float lX, float rX, float Y) {
	if (meter != nullptr) {
		Rect box = meter->LayoutBounds;
		float x = std::fmaxf(lX, joint_x - box.Width * 0.5F);
		float y = joint_y;

		if (x + box.Width >= rX) {
			x = rX - box.Width;
		}

		if (y + box.Height >= Y) {
			y = joint_y - box.Height;
		}

		ds->DrawTextLayout(meter, x, y, this->meter_color);
	}
}

bool DragXZlet::position_equal(float3& old_pos, float3& new_pos) {
	return ((old_pos.x == new_pos.x) && (old_pos.z == new_pos.z));
}
