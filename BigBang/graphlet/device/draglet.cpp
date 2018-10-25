#include "graphlet/device/draglet.hpp"

#include "math.hpp"
#include "shape.hpp"
#include "polar.hpp"
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
static CanvasSolidColorBrush^ drag_default_angle_pointer_color = Colours::Silver;
static CanvasSolidColorBrush^ drag_default_suction_depth_pointer_color = Colours::Yellow;
static CanvasSolidColorBrush^ drag_default_draghead_depth_pointer_color = Colours::Cyan;
static CanvasSolidColorBrush^ drag_default_hatchmark_color = Colours::Silver;

static float drag_hatchmark_fontsize = 24.0F;

static inline float arm_atan(float radius, float half_thickness, float* extended_radius = nullptr) {
	float radians = std::atan2(half_thickness, radius);
	float ext_radius = radius / std::cosf(radians);

	SET_BOX(extended_radius, ext_radius);

	return radians;
}

static CanvasGeometry^ make_draghead(float radius, float bottom_radius, float arm_thickness, float arm_length
	, double offset, double degrees, float sign, float thickness) {
	auto head = ref new CanvasPathBuilder(CanvasDevice::GetSharedDevice());
	float center_radius = radius * 0.2718F;
	float bottom_far_radius = arm_length * 0.72F;
	float arm_joint_x = arm_length - thickness * 0.5F;
	float arm_joint_length = arm_thickness * 1.5F;
	float arm_top_radius = arm_length * 0.618F;
	float arm_bottom_radius = arm_length * 0.85F;
	float arm_half_thickness = arm_thickness * 0.5F;
	float arm_diffradians = degrees_to_radians(offset);
	float radians = degrees_to_radians(degrees);
	float top_start_radians = degrees_to_radians(degrees - 100.0 * sign);
	float top_end_radians = degrees_to_radians(degrees - offset * sign);
	float bottom_start_radians = degrees_to_radians(degrees + 90.0 * sign);
	float bottom_sweep_radians = degrees_to_radians(90.0 * sign);
	float arm_start_radians = degrees_to_radians(degrees + 8.0 * sign);
	float bottom_far_radians = radians + arm_atan(bottom_far_radius, bottom_radius, &bottom_far_radius) * sign;
	float arm_near_top_delta_radians = arm_atan(arm_top_radius, arm_half_thickness, &arm_top_radius);
	float arm_far_delta_radians = arm_atan(arm_length, arm_half_thickness, &arm_length);
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
	circle_point(arm_bottom_radius, arm_start_radians, &arm_near_bottom.x, &arm_near_bottom.y);
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

static CanvasGeometry^ make_visor(float radius, float bottom_radius, float teeth_length, double degrees, float sign) {
	auto visor = ref new CanvasPathBuilder(CanvasDevice::GetSharedDevice());
	float jaw_length = teeth_length * 0.90F;
	float bottom_base_radians = degrees_to_radians(degrees + 90.0 * sign);
	float bottom_diffradians = std::acos(bottom_radius / radius);
	float bottom_radians = bottom_base_radians + bottom_diffradians * sign;
	float bottom_intermediate_radians = degrees_to_radians(degrees + 175.0 * sign);
	float radians = degrees_to_radians(degrees + 180.0 * sign);
	float top_intermediate_radians = degrees_to_radians(degrees + 190.0 * sign);
	float top_start_radians = degrees_to_radians(degrees + 220.0 * sign); // TODO: this angle should based on the range
	float top_stop_radians = degrees_to_radians((sign > 0.0F) ? 280.0 : -80.0);
	float jaw_radians = bottom_base_radians + std::acos(bottom_radius / jaw_length) * sign;
	float teeth_radians = jaw_radians - degrees_to_radians(3.82) * sign;
	float2 bottom_start, bottom_teeth, teeth, bottom_intermediate, top_teeth, top_intermediate_far, top_intermediate_near, top_start;

	circle_point(radius, bottom_radians, &bottom_start.x, &bottom_start.y);
	circle_point(jaw_length, jaw_radians, &bottom_teeth.x, &bottom_teeth.y);
	circle_point(jaw_length, teeth_radians, &teeth.x, &teeth.y);
	circle_point(teeth_length, bottom_intermediate_radians, &bottom_intermediate.x, &bottom_intermediate.y);
	circle_point(teeth_length, radians, &top_teeth.x, &top_teeth.y);
	circle_point(teeth_length, top_intermediate_radians, &top_intermediate_far.x, &top_intermediate_far.y);
	circle_point(jaw_length, top_intermediate_radians, &top_intermediate_near.x, &top_intermediate_near.y);
	circle_point(radius, top_start_radians, &top_start.x, &top_start.y);
	
	visor->BeginFigure(0.0F, 0.0F);
	visor->AddLine(bottom_start);
	visor->AddLine(bottom_teeth);
	visor->AddLine(teeth);
	visor->AddLine(bottom_intermediate);
	visor->AddLine(top_teeth);
	visor->AddLine(top_intermediate_far);
	visor->AddLine(top_intermediate_near);
	visor->AddLine(top_start);
	visor->AddArc(float2(0.0F, 0.0F), radius, radius, top_start_radians, top_stop_radians - top_start_radians);
	visor->EndFigure(CanvasFigureLoop::Closed);

	return CanvasGeometry::CreatePath(visor);
}

/*************************************************************************************************/
float WarGrey::SCADA::drag_depth(WarGrey::SCADA::DragInfo& info) {
	float depth = info.trunnion_length + info.pipe_padding;

	for (unsigned int idx = 0; idx < sizeof(info.pipe_lengths) / sizeof(float); idx++) {
		depth += info.pipe_lengths[idx];
	}

	return depth;
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
		hm = hbhatchmark(hm_width, this->outside_most, this->inside_most, this->step, this->thickness, &metrics, 0U, true);

		this->ws_x = metrics.hatch_x;
		this->ws_width = metrics.hatch_width;
	} else {
		hm = hbhatchmark(hm_width, this->inside_most, this->outside_most, this->step, this->thickness, &metrics, 0U, true);

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
		auto head = vlhatchmark(head_height, this->depth_lowest, this->depth_highest, head_step, this->thickness, &hmetrics, 0U, true);
		auto tail = vrhatchmark(tail_height, this->suction_lowest, this->depth_highest, tail_step, this->thickness, &tmetrics, 0U, true);

		this->hatchmarks = geometry_freeze(geometry_union(head, tail, this->width - tmetrics.width, 0.0F));
		
		this->left_margin = hmetrics.width;
		this->right_margin = this->width - tmetrics.width;
		this->ws_x = this->right_margin - tmetrics.em * 1.618F;
		this->ws_width = (this->left_margin + hmetrics.em * 1.618F) - this->ws_x;
	} else {
		auto head = vrhatchmark(head_height, this->depth_lowest, this->depth_highest, head_step, this->thickness, &hmetrics, 0U, true);
		auto tail = vlhatchmark(tail_height, this->suction_lowest, this->depth_highest, tail_step, this->thickness, &tmetrics, 0U, true);

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
DragHeadlet::DragHeadlet(float radius, unsigned int color, float depth_range, double visor_range, double arm_range
	, float thickness, ICanvasBrush^ bcolor, ICanvasBrush^ acolor, ICanvasBrush^ sdcolor, ICanvasBrush^ ddcolor, ICanvasBrush^ hmcolor)
	: radius(std::fabsf(radius)), thickness(thickness), sign((radius < 0.0F) ? 1.0F : -1.0F)
	, depth_interval(10.0F), visor_range(visor_range), arm_range(arm_range), offset(30.0)
	, visor_color(Colours::make(color))
	, body_color(bcolor == nullptr ? drag_default_head_color : bcolor)
	, angle_pointer_color(acolor == nullptr ? drag_default_angle_pointer_color : acolor)
	, suction_pointer_color(sdcolor == nullptr ? drag_default_suction_depth_pointer_color : sdcolor)
	, draghead_pointer_color(ddcolor == nullptr ? drag_default_draghead_depth_pointer_color : ddcolor)
	, hatchmark_color(hmcolor == nullptr ? drag_default_hatchmark_color : hmcolor) {
	this->pointer_style = make_dash_stroke(CanvasDashStyle::Dash);
	this->depth_range = std::ceilf(depth_range / depth_interval) * depth_interval;
}

void DragHeadlet::fill_extent(float x, float y, float* w, float* h) {
	SET_BOXES(w, h, this->radius * 2.0F);
}

void DragHeadlet::fill_margin(float x, float y, float* ts, float* rs, float* bs, float* ls) {
	SET_BOXES(ls, rs, 0.0F);
	SET_BOXES(ts, bs, this->hspace);
}

void DragHeadlet::construct() {
	RHatchMarkMetrics vmetrics, ametrics;
	VHatchMarkMetrics dmetrics;
	float aradius = this->radius * 0.90F;
	float vradius = this->radius * 0.50F;
	unsigned int depth_step = ((unsigned int)std::round(this->depth_range / depth_interval)) + 1;
	double adeg0 = ((this->sign > 0.0F) ? 0.0 : 180.0);
	double adegn = ((this->sign > 0.0F) ? -this->arm_range : 180.0 + this->arm_range);
	double vdeg0 = ((this->sign > 0.0F) ? -this->offset : 180.0 + this->offset);
	double vdegn = ((this->sign > 0.0F) ? -100.0 : 280.0);
	double degrees = ((this->sign > 0.0F) ? 0.0 : 180.0);
	auto ahatchmark = rhatchmark(aradius, adeg0, adegn, 0.0, this->arm_range, 0U, this->thickness, &ametrics, 0U, true);
	auto vhatchmark = rhatchmark(vradius, vdeg0, vdegn, 0.0, this->visor_range, 0U, this->thickness, &vmetrics, 0U, true);
	float head_radius = vmetrics.ring_radius - vmetrics.ch * 0.618F;
	float arm_thickness = head_radius * 0.618F * 2.0F;
	
	this->hspace = this->radius + ametrics.label_ty;
	this->translate_x = this->radius - this->radius * 2.0F * 0.382F;
	this->translate_y = 0.0F;
	this->visor_radius = head_radius - (vmetrics.ring_radius - head_radius) * 0.618F;
	this->bottom_radius = head_radius * 0.618F;
	this->depth_font = make_bold_text_format(ametrics.em * 0.85F);

	this->arrow_radius = ametrics.ch * 0.5F;
	this->visor_pointer_radius = vmetrics.ring_radius - this->arrow_radius;
	this->arm_pointer_radius = ametrics.ring_radius - this->arrow_radius;

	this->draghead = geometry_freeze(make_draghead(head_radius, this->bottom_radius, arm_thickness, this->radius + this->translate_x,
		this->offset, degrees, this->sign, this->thickness));

	if (this->sign > 0.0F) {
		float height = -ametrics.label_ty - this->bottom_radius + this->translate_y;
		auto dhatchmark = vrhatchmark(height, -this->depth_range, depth_interval, depth_step, 1.0F, &dmetrics, 0U, true, this->depth_font);
		float arrow_length = dmetrics.mark_width;
		auto arrow = hline(arrow_length);
		auto arrowhead = polar_arrowhead(this->arrow_radius, 0.0);
		
		this->depth_pointer = geometry_freeze(geometry_translate(geometry_union(arrowhead, arrow, -arrow_length), -this->arrow_radius));
		this->hatchmarks = geometry_freeze(geometry_union(geometry_union(vhatchmark, ahatchmark), dhatchmark, 0.0F, this->bottom_radius));
	} else {
		float height = -ametrics.label_ty - this->bottom_radius + this->translate_y;
		auto dhatchmark = vlhatchmark(height, -this->depth_range, depth_interval, depth_step, 1.0F, &dmetrics, 0U, true, this->depth_font);
		auto arrow = hline(dmetrics.mark_width);
		auto arrowhead = polar_arrowhead(this->arrow_radius, 180.0);
		
		this->depth_pointer = geometry_freeze(geometry_translate(geometry_union(arrowhead, arrow), this->arrow_radius));
		this->hatchmarks = geometry_freeze(geometry_union(geometry_union(vhatchmark, ahatchmark), dhatchmark,
			-dmetrics.width, this->bottom_radius));
	}

	this->depth_top = this->bottom_radius + dmetrics.hatch_y;
	this->depth_height = dmetrics.hatch_height;

	float3 dh;
	this->set_position(0.0F, nullptr, dh, true);
}

void DragHeadlet::draw(CanvasDrawingSession^ ds, float x, float y, float Width, float Height) {
	float cx = x + this->radius - this->translate_x * this->sign;
	float cy = y + this->radius - this->translate_y;

	ds->DrawCachedGeometry(this->visor, cx, cy, this->visor_color);
	ds->DrawCachedGeometry(this->draghead, cx, cy, this->body_color);
	ds->DrawCachedGeometry(this->hatchmarks, cx, cy, this->hatchmark_color);

	{ // draw pointers
		float depth_range = this->depth_interval + this->depth_range;
		float suction_dy = (this->depth_interval - this->suction_depth) / depth_range * this->depth_height + this->depth_top;
		float draghead_dy = (this->depth_interval - this->draghead_depth) / depth_range * this->depth_height + this->depth_top;

		ds->DrawCachedGeometry(this->visor_pointer, cx, cy, this->angle_pointer_color);
		ds->DrawCachedGeometry(this->arm_pointer, cx, cy, this->angle_pointer_color);

		ds->DrawCachedGeometry(this->depth_pointer, cx, cy + draghead_dy, this->draghead_pointer_color);
		ds->DrawCachedGeometry(this->depth_pointer, cx, cy + suction_dy, this->suction_pointer_color);
	}
}

void DragHeadlet::set_position(float suction_depth, float3 ujoints[], float3& draghead, bool force) {
	float visor_length = this->radius - this->translate_x;
	
	this->suction_depth = -suction_depth;
	this->draghead_depth = -draghead.z;
	this->visor_degrees = ((this->sign > 0.0F) ? 0.0 : 180.0);
	this->arm_degrees = ((this->sign > 0.0F) ? 0.0 : 180.0);

	this->visor = geometry_freeze(make_visor(this->visor_radius, this->bottom_radius, visor_length, this->visor_degrees, this->sign));

	{ // make pointers
		float vx, vy, ax, ay;
		double vdeg = this->visor_degrees - this->offset * this->sign;
		auto vhead = polar_arrowhead(this->arrow_radius, vdeg);
		auto ahead = polar_arrowhead(this->arrow_radius, this->arm_degrees);

		circle_point(this->visor_pointer_radius, vdeg, &vx, &vy);
		circle_point(this->arm_pointer_radius, this->arm_degrees, &ax, &ay);

		this->visor_pointer = geometry_freeze(geometry_union(line(vx, vy, 1.0F, this->pointer_style), vhead, vx, vy));
		this->arm_pointer = geometry_freeze(geometry_union(line(ax, ay, 1.0F, this->pointer_style), ahead, ax, ay));
	}
}
