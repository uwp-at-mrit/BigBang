﻿#include "graphlet/device/draglet.hpp"

#include "math.hpp"
#include "shape.hpp"
#include "paint.hpp"
#include "geometry.hpp"

#include "string.hpp"

#include "measure/vhatchmark.hpp"
#include "measure/hhatchmark.hpp"
#include "measure/rhatchmark.hpp"

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

static float drag_hatchmark_fontsize = 24.0F;

static inline float arm_adjust_outline_radian(float radius, float half_thickness, float* extended_radius = nullptr) {
	float radians = std::atan2(half_thickness, radius);
	float ext_radius = radius / std::cosf(radians);

	SET_BOX(extended_radius, ext_radius);

	return radians;
}

static CanvasGeometry^ make_draghead(float radius, float arm_thickness, float arm_length, double offset, double degrees
	, float sign, float thickness) {
	auto head = ref new CanvasPathBuilder(CanvasDevice::GetSharedDevice());
	float center_radius = radius * 0.2718F;
	float bottom_radius = radius * 0.618F;
	float bottom_far_radius = arm_length * 0.72F;
	float arm_joint_x = arm_length - thickness * 0.5F;
	float arm_joint_length = arm_thickness * 1.5F;
	float arm_top_radius = arm_length * 0.618F;
	float arm_near_radius = arm_length * 0.85F;
	float arm_half_thickness = arm_thickness * 0.5F;
	float arm_diffradians = degrees_to_radians(offset);
	float radians = degrees_to_radians(degrees);
	float top_start_radians = degrees_to_radians(degrees - 100.0 * sign);
	float top_end_radians = degrees_to_radians(degrees - offset * sign);
	float bottom_start_radians = degrees_to_radians(degrees + 90.0 * sign);
	float bottom_sweep_radians = degrees_to_radians(90.0 * sign);
	float arm_start_radians = degrees_to_radians(degrees + 8.0 * sign);
	float bottom_far_radians = radians + arm_adjust_outline_radian(bottom_far_radius, bottom_radius, &bottom_far_radius) * sign;
	float arm_near_top_delta_radians = arm_adjust_outline_radian(arm_top_radius, arm_half_thickness, &arm_top_radius);
	float arm_far_delta_radians = arm_adjust_outline_radian(arm_length, arm_half_thickness, &arm_length);
	float arm_near_top_radians = radians - arm_near_top_delta_radians * sign;
	float arm_far_top_radians = radians - arm_far_delta_radians * sign;
	float arm_far_bottom_radians = radians + arm_far_delta_radians * sign;
	float2 top_start, arm_near_top, arm_far_top, arm_near_bottom, arm_far_bottom, bottom_far, bottom_near_start;
	CanvasGeometry^ center = circle(center_radius);
	CanvasGeometry^ joint = vline(arm_joint_x, -arm_joint_length * 0.5F, arm_joint_length, thickness);

	circle_point(radius, top_start_radians, &top_start.x, &top_start.y);
	circle_point(arm_top_radius, arm_near_top_radians, &arm_near_top.x, &arm_near_top.y);
	circle_point(arm_length, arm_far_top_radians, &arm_far_top.x, &arm_far_top.y);
	circle_point(arm_length, arm_far_bottom_radians, &arm_far_bottom.x, &arm_far_bottom.y);
	circle_point(arm_near_radius, arm_start_radians, &arm_near_bottom.x, &arm_near_bottom.y);
	circle_point(bottom_far_radius, bottom_far_radians, &bottom_far.x, &bottom_far.y);
	circle_point(bottom_radius, bottom_start_radians, &bottom_near_start.x, &bottom_near_start.y);

	head->BeginFigure(top_start);
	head->AddArc(float2(0.0F, 0.0F), radius, radius, top_start_radians, top_end_radians - top_start_radians);
	head->AddLine(arm_near_top);
	head->AddLine(arm_far_top);
	head->AddLine(arm_far_bottom);
	head->AddLine(arm_near_bottom);
	head->AddLine(bottom_far);
	head->AddLine(bottom_near_start);
	head->AddArc(float2(0.0F, 0.0F), bottom_radius, bottom_radius, bottom_start_radians, bottom_sweep_radians);
	head->AddLine(0.0F, 0.0F);
	head->AddLine(top_start);
	head->EndFigure(CanvasFigureLoop::Closed);

	return geometry_union(CanvasGeometry::CreatePath(head), geometry_union(center, geometry_rotate(joint, degrees, 0.0F, 0.0F)));
}

static CanvasGeometry^ make_visor(float radius, float teeth_length, double degrees, float sign) {
	auto visor = ref new CanvasPathBuilder(CanvasDevice::GetSharedDevice());
	float radians = degrees_to_radians(degrees);
	float bottom_radians = degrees_to_radians(degrees + 135.0 * sign);
	float top_end_radians = degrees_to_radians(280.0 * sign);
	float2 top_start, arm_start, arm_stop, bottom_start;

	circle_point(radius, bottom_radians, &bottom_start.x, &bottom_start.y);
	
	visor->BeginFigure(0.0F, 0.0F);
	visor->AddLine(bottom_start);
	visor->AddArc(float2(0.0F, 0.0F), radius, radius, bottom_radians, top_end_radians - bottom_radians);
	visor->EndFigure(CanvasFigureLoop::Closed);

	return CanvasGeometry::CreatePath(visor);
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

	this->total_length = this->info.trunnion_length;
	for (unsigned int idx = 0; idx < sizeof(this->info.pipe_lengths) / sizeof(float); idx++) {
		this->ujoints[idx].x = this->total_length;
		this->ujoints[idx].y = this->info.trunnion_gapsize;

		if (idx == 1) {
			this->total_length += this->info.pipe_lengths[idx] + this->info.pipe_padding;
		} else {
			this->total_length += this->info.pipe_lengths[idx];
		}
	}

	this->draghead.x = this->total_length;
	this->draghead.y = this->info.trunnion_gapsize;

	this->suction_style = make_dash_stroke(CanvasDashStyle::Dash);
	this->dragarm_style = make_round_stroke_style();

	if (this->mfont == nullptr) {
		this->mfont = make_text_format(drag_hatchmark_fontsize);
	}
}

void IDraglet::update(long long count, long long interval, long long uptime) {
	if (this->dredging) {
		this->suction_style->DashOffset = float(count);
		this->notify_updated();
	}
}

void IDraglet::fill_extent(float x, float y, float* w, float* h) {
	SET_VALUES(w, this->width, h, this->height);
}

void IDraglet::set_position(float suction_depth, float3 ujoints[], float3& draghead, bool force) {
	bool schanged = (this->suction.z != suction_depth);
	bool dchanged = (!this->position_equal(this->draghead, draghead));
	bool ichanged = false;

	if (schanged) {
		this->suction.z = suction_depth;
	}

	if (dchanged) {
		this->draghead = draghead;
	}

	for (unsigned int idx = 0; idx < DRAG_SEGMENT_MAX_COUNT; idx++) {
		if (this->info.pipe_lengths[idx] > 0.0) {
			if (!this->position_equal(this->ujoints[idx], ujoints[idx])) {
				this->ujoints[idx] = ujoints[idx];
				ichanged = true;
			}
		}
	}

	if (force || schanged || ichanged || dchanged) {
		CanvasPathBuilder^ arm = ref new CanvasPathBuilder(CanvasDevice::GetSharedDevice());
		float3 _trunnion;

		this->_suction = this->space_to_local(this->suction);
		this->_draghead = this->space_to_local(draghead);
		this->draghead_m = make_text_layout(this->position_label(draghead), this->mfont);

		_trunnion.x = 0.0F;
		_trunnion.y = this->info.trunnion_gapsize;
		_trunnion.z = suction_depth;

		arm->BeginFigure(this->_suction);
		arm->AddLine(this->space_to_local(_trunnion));

		for (unsigned int idx = 0; idx < DRAG_SEGMENT_MAX_COUNT; idx++) {
			if (this->info.pipe_lengths[idx] > 0.0F) {
				this->rubbers[idx] = this->universal_joint;
				this->_ujoints[idx] = this->space_to_local(ujoints[idx]);
				this->ujoints_ms[idx] = make_text_layout(this->position_label(ujoints[idx]), this->mfont);
				
				arm->AddLine(this->_ujoints[idx]);
			}
		}

		arm->AddLine(this->_draghead);
		arm->EndFigure(CanvasFigureLoop::Open);

		this->dragarm = CanvasGeometry::CreatePath(arm);
		this->on_position_changed(this->suction.z, this->ujoints, this->draghead);
		this->notify_updated();
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
		float axis_by = this->height - metrics.height;
		auto zero = vline(axis_by, this->thickness);
		
		this->_suction = this->space_to_local(this->suction);
		this->hatchmarks = geometry_freeze(geometry_union(hm, 0.0F, axis_by, zero, this->_suction.x, 0.0));
	}
	
	{ // make drag
		this->ws_y = metrics.height * 1.618F;
		this->ws_height = this->height - this->ws_y * 2.0F - metrics.height;
		this->drag_thickness = this->ws_height * (this->info.pipe_radius * 2.0F) / this->total_length;
		this->joint_radius = this->drag_thickness * 0.618F;

		this->universal_joint = circle(this->joint_radius);
		
		this->set_position(0.0F, this->ujoints, this->draghead, true);
	}
}

void DragXYlet::draw(CanvasDrawingSession^ ds, float x, float y, float Width, float Height) {
	float draghead_x = x + this->_draghead.x;
	float draghead_y = y + this->_draghead.y;
	
	ds->DrawCachedGeometry(this->hatchmarks, x, y, this->hatchmark_color);

	{ // draw drag
		ds->DrawGeometry(this->dragarm, x, y, this->body_color, this->drag_thickness, this->dragarm_style);

		for (int idx = DRAG_SEGMENT_MAX_COUNT - 1; idx >= 0; idx--) {
			if (this->info.pipe_lengths[idx] > 0.0F) {
				float ix = x + this->_ujoints[idx].x;
				float iy = y + this->_ujoints[idx].y;

				if (this->rubbers[idx] != nullptr) {
					ds->FillGeometry(this->rubbers[idx], ix, iy, this->color);
					ds->DrawGeometry(this->rubbers[idx], ix, iy, this->body_color);
				}
			}
		}

		{ // draw draghead
			ds->FillGeometry(this->universal_joint, draghead_x, draghead_y, this->color);
			ds->DrawGeometry(this->universal_joint, draghead_x, draghead_y, this->body_color, this->thickness);
		}

		ds->DrawGeometry(this->dragarm, x, y, this->color, 1.0F, this->suction_style);
	}

	{ // draw meters
		for (unsigned int idx = 1; idx < DRAG_SEGMENT_MAX_COUNT; idx++) {
			if (this->info.pipe_lengths[idx] > 0.0F) {
				float ix = x + this->_ujoints[idx].x;
				float iy = y + this->_ujoints[idx].y;

				this->draw_meter(ds, this->ujoints_ms[idx], ix, iy, x);
			}
		}

		this->draw_meter(ds, this->draghead_m, draghead_x, draghead_y, x);
	}
}

void DragXYlet::draw_meter(CanvasDrawingSession^ ds, CanvasTextLayout^ meter, float joint_x, float joint_y, float gx) {
	if (meter != nullptr) {
		Rect box = meter->LayoutBounds;
		float x0 = this->_suction.x;
		float lx0 = x0 - this->thickness;
		float rx0 = x0 + this->thickness;
		float y = joint_y - box.Height * 0.5F;
		float x = joint_x + (this->leftward ? (-box.Width - this->drag_thickness) : this->drag_thickness);
		float rx = x + box.Width;

		if ((x < gx) || ((x < x0) && (rx > rx0))) {
			x = joint_x + this->drag_thickness;
		}

		if (rx > gx + this->width) {
			x = joint_x - box.Width - this->drag_thickness;
		}

		ds->DrawTextLayout(meter, x, y, this->meter_color);
	}
}

bool DragXYlet::position_equal(float3& old_pos, float3& new_pos) {
	return ((old_pos.x == new_pos.x) && (old_pos.y == new_pos.y));
}

Platform::String^ DragXYlet::position_label(float3& position) {
	return flstring(position.y, this->precision);
}

float2 DragXYlet::space_to_local(float3& position) {
	float px = position.x / this->total_length;
	float py = float(this->outside_most - position.y) / float(this->outside_most - this->inside_most);
	float2 location;

	location.x = this->ws_width * py + this->ws_x;
	location.y = this->ws_height * px + this->ws_y;
	
	return location;
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
	
	if (this->leftward) {
		auto head = vlhatchmark(head_height, this->depth_lowest, this->depth_highest, head_step, this->thickness, &hmetrics);
		auto tail = vrhatchmark(tail_height, this->suction_lowest, this->depth_highest, tail_step, this->thickness, &tmetrics);

		this->hatchmarks = geometry_freeze(geometry_union(head, tail, this->width - tmetrics.width, 0.0F));
		
		this->left_margin = hmetrics.width;
		this->right_margin = this->width - tmetrics.width;
		this->ws_x = this->right_margin - tmetrics.em * 1.618F;
		this->ws_width = (this->left_margin + hmetrics.em * 1.618F) - this->ws_x;
	} else {
		auto head = vrhatchmark(head_height, this->depth_lowest, this->depth_highest, head_step, this->thickness, &hmetrics);
		auto tail = vlhatchmark(tail_height, this->suction_lowest, this->depth_highest, tail_step, this->thickness, &tmetrics);

		this->hatchmarks = geometry_freeze(geometry_union(head, this->width - hmetrics.width, 0.0F, tail));
		
		this->left_margin = tmetrics.width;
		this->right_margin = this->width - hmetrics.width;
		this->ws_x = this->left_margin + tmetrics.em * 1.618F;
		this->ws_width = (this->right_margin - hmetrics.em * 1.618F) - this->ws_x;
	}

	{ // make draghead keypoint;
		this->ws_y = hmetrics.hatch_y;
		this->ws_height = hmetrics.hatch_height;
		this->drag_thickness = std::fabsf(this->ws_width) * (this->info.pipe_radius * 2.0F) / this->total_length;
		this->joint_radius = this->drag_thickness * 0.618F;

		this->universal_joint = circle(this->joint_radius);

		this->set_position(0.0F, this->ujoints, this->draghead, true);
	}
}

void DragXZlet::on_position_changed(float suction_depth, float3 ujoints[], float3& draghead) {
	this->suction_m = make_text_layout(flstring(suction_depth, this->precision), this->mfont);
}

void DragXZlet::draw(CanvasDrawingSession^ ds, float x, float y, float Width, float Height) {
	float suction_x = x + this->_suction.x;
	float suction_y = y + this->_suction.y;
	float draghead_x = x + this->_draghead.x;
	float draghead_y = y + this->_draghead.y;
	
	ds->DrawCachedGeometry(this->hatchmarks, x, y, this->hatchmark_color);

	{ // draw drag
		ds->DrawGeometry(this->dragarm, x, y, this->body_color, this->drag_thickness, this->dragarm_style);

		for (int idx = DRAG_SEGMENT_MAX_COUNT - 1; idx >= 0; idx--) {
			if (this->info.pipe_lengths[idx] > 0.0F) {
				float ix = x + this->_ujoints[idx].x;
				float iy = y + this->_ujoints[idx].y;

				if (this->rubbers[idx] != nullptr) {
					ds->FillGeometry(this->rubbers[idx], ix, iy, this->color);
					ds->DrawGeometry(this->rubbers[idx], ix, iy, this->body_color);
				}
			}
		}

		ds->FillGeometry(this->universal_joint, suction_x, suction_y, this->color);
		ds->DrawGeometry(this->universal_joint, suction_x, suction_y, this->body_color, this->thickness);

		{ // draw draghead
			ds->FillGeometry(this->universal_joint, draghead_x, draghead_y, this->color);
			ds->DrawGeometry(this->universal_joint, draghead_x, draghead_y, this->body_color, this->thickness);
		}

		ds->DrawGeometry(this->dragarm, x, y, this->color, 1.0F, this->suction_style);
	}
	
	{ // draw meters
		float lX = x + this->left_margin + this->thickness;
		float rX = x + this->right_margin - this->thickness;
		float Y = y + this->height;
	
		for (unsigned int idx = 1; idx < DRAG_SEGMENT_MAX_COUNT; idx++) {
			if (this->info.pipe_lengths[idx] > 0.0F) {
				float ix = x + this->_ujoints[idx].x;
				float iy = y + this->_ujoints[idx].y;

				this->draw_meter(ds, this->ujoints_ms[idx], ix, iy, lX, rX, Y);
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

Platform::String^ DragXZlet::position_label(float3& position) {
	return flstring(position.z, this->precision);
}

float2 DragXZlet::space_to_local(float3& position) {
	float depth = -position.z;
	float px = position.x / this->total_length;
	float py = float(this->depth_highest - depth) / float(this->depth_highest - this->depth_lowest);
	float2 location;

	location.x = this->ws_width * px + this->ws_x;
	location.y = this->ws_height * py + this->ws_y;

	return location;
}

/*************************************************************************************************/
DragHeadlet::DragHeadlet(float radius, unsigned int color, double range, float thickness, ICanvasBrush^ bcolor, ICanvasBrush^ hmcolor)
	: radius(std::fabsf(radius)), thickness(thickness), precision(0U), leftward(radius < 0.0F), range(range), offset(30.0)
	, visor_color(Colours::make(color))
	, body_color(bcolor == nullptr ? drag_default_head_color : bcolor)
	, hatchmark_color(hmcolor == nullptr ? drag_default_hatchmark_color : hmcolor) {}

void DragHeadlet::fill_extent(float x, float y, float* w, float* h) {
	SET_BOXES(w, h, this->radius * 2.0F);
}

void DragHeadlet::construct() {
	RHatchMarkMetrics vmetrics, ametrics;
	float aradius = this->radius * 0.90F;
	float vradius = this->radius * 0.50F;
	double adeg0 = (this->leftward ? 0.0 : 180.0);
	double adegn = (this->leftward ? -this->range : 180.0 + this->range);
	double vdeg0 = (this->leftward ? -this->offset : 180.0 + this->offset);
	double vdegn = (this->leftward ? -100.0 : 280.0);
	double degrees = (this->leftward ? 0.0 : 180.0);
	auto ahatchmark = rhatchmark(aradius, adeg0, adegn, 0.0, this->range, 0U, this->thickness, &ametrics, this->precision);
	auto vhatchmark = rhatchmark(vradius, vdeg0, vdegn, 0.0, this->range, 0U, this->thickness, &vmetrics, this->precision);
	float head_radius = vmetrics.ring_radius - vmetrics.ch * 0.618F;
	float arm_thickness = head_radius * 0.618F * 2.0F;

	this->hatchmarks = geometry_freeze(geometry_union(vhatchmark, ahatchmark));
	this->visor_radius = head_radius - (vmetrics.ring_radius - head_radius);
	this->draghead = make_draghead(head_radius, arm_thickness, this->radius, this->offset, degrees,
		(this->leftward ? 1.0F : -1.0F), this->thickness);

	float3 dh;
	this->set_position(0.0F, nullptr, dh, true);
}

void DragHeadlet::draw(CanvasDrawingSession^ ds, float x, float y, float Width, float Height) {
	float cx = x + this->radius;
	float cy = y + this->radius;

	ds->DrawRectangle(x, y, this->radius * 2.0F, this->radius * 2.0F, Colours::Crimson);

	ds->FillGeometry(this->visor, cx, cy, this->visor_color);
	ds->FillGeometry(this->draghead, cx, cy, this->body_color);
	ds->DrawCachedGeometry(this->hatchmarks, cx, cy, this->hatchmark_color);
}

void DragHeadlet::set_position(float suction_depth, float3 ujoints[], float3& draghead, bool force) {
	float degrees = (this->leftward ? 0.0F : 180.0F);
	this->visor = make_visor(this->visor_radius, this->radius, degrees, (this->leftward ? 1.0F : -1.0F));
}
