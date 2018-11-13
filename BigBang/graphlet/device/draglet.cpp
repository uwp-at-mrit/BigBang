#include "graphlet/device/draglet.hpp"

#include "math.hpp"
#include "shape.hpp"
#include "paint.hpp"
#include "polar.hpp"
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
static CanvasSolidColorBrush^ drag_default_angle_pointer_color = Colours::GhostWhite;
static CanvasSolidColorBrush^ drag_default_draghead_depth_pointer_color = Colours::Cyan;
static CanvasSolidColorBrush^ drag_default_suction_depth_pointer_color = Colours::Yellow;
static CanvasSolidColorBrush^ drag_default_hatchmark_color = Colours::Silver;

static float drag_hatchmark_fontsize = 24.0F;
static double drag_visor_end_angle = 100.0;

static inline double drag_adjusted_angle(double degrees, float sign) {
	return ((sign > 0.0F) ? 0.0 - degrees : 180.0 + degrees);
}

static inline float drag_arm_atan(float radius, float half_thickness, float* extended_radius = nullptr) {
	float radians = std::atan2(half_thickness, radius);
	float ext_radius = radius / std::cosf(radians);

	SET_BOX(extended_radius, ext_radius);

	return radians;
}

static CanvasGeometry^ make_draghead(float radius, float bottom_radius, float arm_thickness, float arm_length
	, double offset, double degrees0, float sign, float extent) {
	auto head = ref new CanvasPathBuilder(CanvasDevice::GetSharedDevice());
	double degrees = drag_adjusted_angle(degrees0, sign);
	float center_radius = radius * 0.2718F;
	float bottom_far_radius = arm_length * 0.72F;
	float arm_joint_thickness = extent * 0.5F;
	float arm_joint_x = arm_length - arm_joint_thickness * 0.5F;
	float arm_joint_length = arm_thickness + extent * 2.0F;
	float arm_top_radius = arm_length * 0.618F;
	float arm_bottom_radius = arm_length * 0.80F;
	float arm_half_thickness = arm_thickness * 0.5F;
	float arm_diffradians = degrees_to_radians(offset);
	float radians = degrees_to_radians(degrees);
	float top_start_radians = degrees_to_radians(degrees - drag_visor_end_angle * sign);
	float top_end_radians = degrees_to_radians(degrees - offset * sign);
	float bottom_start_radians = degrees_to_radians(degrees + 90.0 * sign);
	float bottom_sweep_radians = degrees_to_radians(90.0 * sign);
	float bottom_far_radians = radians + drag_arm_atan(bottom_far_radius, bottom_radius, &bottom_far_radius) * sign;
	float arm_start_radians = degrees_to_radians(degrees + 10.0 * sign);
	float arm_near_top_delta_radians = drag_arm_atan(arm_top_radius, arm_half_thickness, &arm_top_radius);
	float arm_far_delta_radians = drag_arm_atan(arm_length, arm_half_thickness, &arm_length);
	float arm_near_top_radians = radians - arm_near_top_delta_radians * sign;
	float arm_far_top_radians = radians - arm_far_delta_radians * sign;
	float arm_far_bottom_radians = radians + arm_far_delta_radians * sign;
	float2 top_start, arm_near_top, arm_far_top, arm_near_bottom, arm_far_bottom, bottom_far, bottom_near_start;
	CanvasGeometry^ center = circle(center_radius);
	CanvasGeometry^ joint = vline(arm_joint_x, -arm_joint_length * 0.5F, arm_joint_length, arm_joint_thickness);

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

static CanvasGeometry^ make_visor(float radius, float bottom_radius, float teeth_length, double degrees0, double arm_degrees0, float sign) {
	auto visor = ref new CanvasPathBuilder(CanvasDevice::GetSharedDevice());
	double degrees = drag_adjusted_angle(degrees0, sign);
	double arm_degrees = drag_adjusted_angle(arm_degrees0, sign);
	float jaw_length = teeth_length * 0.85F;
	float bottom_base_radians = degrees_to_radians(degrees + 90.0 * sign);
	float bottom_diffradians = std::acos(bottom_radius / radius);
	float bottom_radians = bottom_base_radians + bottom_diffradians * sign;
	float bottom_intermediate_radians = degrees_to_radians(degrees + 175.0 * sign);
	float radians = degrees_to_radians(degrees + 180.0 * sign);
	float top_intermediate_radians = degrees_to_radians(degrees + 190.0 * sign);
	float top_start_radians = degrees_to_radians(degrees + 215.0 * sign); // TODO: this angle should based on the range
	float top_stop_radians = degrees_to_radians(arm_degrees + drag_visor_end_angle + ((sign > 0.0F) ? 180.0 : -360.0));
	float jaw_radians = bottom_base_radians + std::acos(bottom_radius / jaw_length) * sign;
	float teeth_radians = jaw_radians - degrees_to_radians(6.18) * sign;
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

static inline CanvasGeometry^ make_pointer(float radius, double degrees, float arrow_radius, CanvasStrokeStyle^ style) {
	auto arrowhead = polar_arrowhead(arrow_radius, degrees);
	float x, y;

	circle_point(radius, degrees, &x, &y);

	return geometry_union(line(x, y, 1.0F, style), arrowhead, x, y);
}

/*************************************************************************************************/
float WarGrey::SCADA::drag_depth(WarGrey::SCADA::DragInfo& info) {
	float depth = info.trunnion_length + info.pipe_padding + info.head_length;

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

	this->drag_length = this->info.trunnion_length;
	for (unsigned int idx = 0; idx < sizeof(this->info.pipe_lengths) / sizeof(float); idx++) {
		this->ujoints[idx].x = this->drag_length;
		this->ujoints[idx].y = this->info.trunnion_gapsize;

		if (idx == 1) {
			this->drag_length += this->info.pipe_lengths[idx] + this->info.pipe_padding;
		} else {
			this->drag_length += this->info.pipe_lengths[idx];
		}
	}

	this->drag_length += info.head_length;
	this->draghead.x = this->drag_length;
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

void IDraglet::set_position(float suction_depth, float3 ujoints[], float3& draghead, double visor_angle, bool force) {
	bool schanged = (this->suction.z != suction_depth);
	bool dchanged = (!this->position_equal(this->draghead, draghead));
	bool vchanged = (this->visor_angle != visor_angle);
	bool ichanged = false;

	this->suction.z = suction_depth;
	this->draghead = draghead;
	this->visor_angle = visor_angle;

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
		unsigned int last_joint_idx = 0;
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
				
				last_joint_idx = idx;

				arm->AddLine(this->_ujoints[idx]);
			}
		}

		arm->AddLine(this->_draghead);
		arm->EndFigure(CanvasFigureLoop::Open);

		this->dragarm = CanvasGeometry::CreatePath(arm);
		this->draghead_angle = points_angle(this->_draghead, this->_ujoints[last_joint_idx]);

		this->on_position_changed(this->suction.z, this->ujoints, this->draghead);
		this->update_drag_head();

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
	float drag_thickness_ratio = (this->info.pipe_radius * 2.0F) / this->drag_length;
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
		this->draghead_length = this->height * drag_thickness_ratio * 3.14F;
		this->ws_y = metrics.height * 1.618F;
		this->ws_height = this->height - this->ws_y - metrics.height * 2.0F - this->draghead_length * 0.382F;
		this->drag_thickness = this->ws_height * drag_thickness_ratio;
		this->joint_radius = this->drag_thickness * 0.618F;

		this->universal_joint = circle(this->joint_radius);
		
		this->set_position(0.0F, this->ujoints, this->draghead, true);
	}
}

void DragXYlet::update_drag_head() {
	float ubase = this->drag_thickness;
	float bbase = this->width * this->info.head_width / this->drag_length;
	float h_height = this->draghead_length * 0.618F;
	double angle = this->draghead_angle + 90.0;
	float sign = (this->leftward ? 1.0F : -1.0F);
	float shape_x = bbase * -0.5F;
	auto hshape = trapezoid(shape_x, -h_height, ubase, bbase, h_height);
	auto vshape = rectangle(shape_x, 0.0F, bbase, this->draghead_length - h_height);

	this->visor_part = geometry_freeze(geometry_rotate(vshape, angle, 0.0F, 0.0F));
	this->draghead_part = geometry_freeze(geometry_rotate(hshape, angle, 0.0F, 0.0F));
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

		ds->DrawGeometry(this->dragarm, x, y, this->color, 1.0F, this->suction_style);

		{ // draw draghead
			ds->FillGeometry(this->universal_joint, draghead_x, draghead_y, this->color);
			ds->DrawGeometry(this->universal_joint, draghead_x, draghead_y, this->body_color, this->thickness);

			ds->DrawCachedGeometry(this->visor_part, draghead_x, draghead_y, this->color);
			ds->DrawCachedGeometry(this->draghead_part, draghead_x, draghead_y, this->head_color);
		}
	}

	{ // draw meters
		for (unsigned int idx = 1; idx < DRAG_SEGMENT_MAX_COUNT; idx++) {
			if (this->info.pipe_lengths[idx] > 0.0F) {
				float ix = x + this->_ujoints[idx].x;
				float iy = y + this->_ujoints[idx].y;

				this->draw_meter(ds, this->ujoints_ms[idx], ix, iy, x, this->drag_thickness);
			}
		}

		this->draw_meter(ds, this->draghead_m, draghead_x, draghead_y, x, this->drag_thickness * 1.618F);
	}
}

void DragXYlet::draw_meter(CanvasDrawingSession^ ds, CanvasTextLayout^ meter, float joint_x, float joint_y, float gx, float hspace) {
	if (meter != nullptr) {
		Rect box = meter->LayoutBounds;
		float x0 = this->_suction.x;
		float lx0 = x0 - this->thickness;
		float rx0 = x0 + this->thickness;
		float y = joint_y - box.Height * 0.5F;
		float x = joint_x + (this->leftward ? (-box.Width - hspace) : hspace);
		float rx = x + box.Width;

		if ((x < gx) || ((x < x0) && (rx > rx0))) {
			x = joint_x + hspace;
		}

		if (rx > gx + this->width) {
			x = joint_x - box.Width - hspace;
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
	float px = position.x / this->drag_length;
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
	this->depth_lowest = -std::ceilf(this->drag_length / interval) * interval;
}

void DragXZlet::construct() {
	VHatchMarkMetrics hmetrics, tmetrics;
	double head_range = this->depth_highest - this->depth_lowest;
	double tail_range = this->depth_highest - this->suction_lowest;
	float head_height = this->height;
	float tail_height = head_height * float(tail_range / head_range) + this->thickness * float(head_range / tail_range);
	unsigned int head_step = ((unsigned int)std::round(head_range / this->depth_highest));
	unsigned int tail_step = ((unsigned int)std::round(tail_range / this->depth_highest));
	float drag_thickness_ratio = (this->info.pipe_radius * 2.0F) / this->drag_length;

	this->draghead_length = this->width * drag_thickness_ratio * 3.14F;
	
	if (this->leftward) {
		auto head = vlhatchmark(head_height, this->depth_lowest, this->depth_highest, head_step, this->thickness, &hmetrics, 0U, true);
		auto tail = vrhatchmark(tail_height, this->suction_lowest, this->depth_highest, tail_step, this->thickness, &tmetrics, 0U, true);

		this->hatchmarks = geometry_freeze(geometry_union(head, tail, this->width - tmetrics.width, 0.0F));
		
		this->left_margin = hmetrics.width;
		this->right_margin = this->width - tmetrics.width;
		this->ws_x = this->right_margin - tmetrics.em * 1.618F;
		this->ws_width = (this->left_margin + hmetrics.em * 1.618F + this->draghead_length * 0.314F) - this->ws_x;
	} else {
		auto head = vrhatchmark(head_height, this->depth_lowest, this->depth_highest, head_step, this->thickness, &hmetrics, 0U, true);
		auto tail = vlhatchmark(tail_height, this->suction_lowest, this->depth_highest, tail_step, this->thickness, &tmetrics, 0U, true);

		this->hatchmarks = geometry_freeze(geometry_union(head, this->width - hmetrics.width, 0.0F, tail));
		
		this->left_margin = tmetrics.width;
		this->right_margin = this->width - hmetrics.width;
		this->ws_x = this->left_margin + tmetrics.em * 1.618F;
		this->ws_width = (this->right_margin - hmetrics.em * 1.618F - this->draghead_length * 0.314F) - this->ws_x;
	}

	{ // make drag
		this->ws_y = hmetrics.hatch_y;
		this->ws_height = hmetrics.hatch_height;
		this->drag_thickness = std::fabsf(this->ws_width) * drag_thickness_ratio;
		this->joint_radius = this->drag_thickness * 0.618F;

		this->universal_joint = circle(this->joint_radius);

		this->set_position(0.0F, this->ujoints, this->draghead, true);
	}
}

void DragXZlet::update_drag_head() {
	float arm_thickness = this->joint_radius * 2.0F;
	float head_radius = this->joint_radius * 1.618F;
	float bottom_radius = head_radius * 0.618F;
	float arm_length = this->draghead_length * 0.618F;
	float visor_radius = head_radius * 0.80F;
	float visor_length = this->draghead_length * 0.5F;
	float sign = (this->leftward ? 1.0F : -1.0F);
	double angle = drag_adjusted_angle(this->draghead_angle, sign);
	double head_joint_start = (this->leftward ? -angle + 90.0 : angle - 90.0);
	double head_joint_end = (this->leftward ? -angle + 270.0 : angle + 90.0);
	auto vshape = make_visor(visor_radius, bottom_radius, visor_length, angle + this->visor_angle, angle, sign);
	auto hshape = make_draghead(head_radius, bottom_radius, arm_thickness, arm_length, 30.0, angle, sign, this->thickness * 2.0F);
	auto mask = sector(head_joint_start, head_joint_end, this->joint_radius + this->thickness);
	
	circle_point(arm_length, this->draghead_angle, &this->mask_dx, &this->mask_dy);

	this->visor_part = geometry_freeze(vshape);
	this->draghead_part = geometry_freeze(hshape);
	this->joint_mask = geometry_freeze(mask);
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

		ds->DrawGeometry(this->dragarm, x, y, this->color, 1.0F, this->suction_style);

		{ // draw draghead
			float draghead_joint_x = draghead_x + this->mask_dx;
			float draghead_joint_y = draghead_y + this->mask_dy;

			ds->FillGeometry(this->universal_joint, draghead_joint_x, draghead_joint_y, this->color);
			ds->DrawGeometry(this->universal_joint, draghead_joint_x, draghead_joint_y, this->body_color, this->thickness);
			ds->DrawCachedGeometry(this->joint_mask, draghead_joint_x, draghead_joint_y, Colours::Background);

			ds->DrawCachedGeometry(this->visor_part, draghead_x, draghead_y, this->color);
			ds->DrawCachedGeometry(this->draghead_part, draghead_x, draghead_y, this->head_color);
		}
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

		//this->draw_meter(ds, this->suction_m, suction_x, suction_y, lX, rX, Y);
		this->draw_meter(ds, this->draghead_m, draghead_x, draghead_y, lX, rX, Y);
	}
}

void DragXZlet::draw_meter(CanvasDrawingSession^ ds, CanvasTextLayout^ meter, float joint_x, float joint_y, float lX, float rX, float Y) {
	if (meter != nullptr) {
		Rect box = meter->LayoutBounds;
		float x = std::fmaxf(lX, joint_x - box.Width * 0.5F);
		float y = joint_y + box.Height * 0.1618F;

		if (x + box.Width >= rX) {
			x = rX - box.Width;
		}

		if (y + box.Height >= Y) {
			y = joint_y - box.Height * 1.1618F;
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
	float px = position.x / this->drag_length;
	float py = float(this->depth_highest - depth) / float(this->depth_highest - this->depth_lowest);
	float2 location;

	location.x = this->ws_width * px + this->ws_x;
	location.y = this->ws_height * py + this->ws_y;

	return location;
}

/*************************************************************************************************/
DragHeadlet::DragHeadlet(float radius, unsigned int color, float depth_range, double visor_range, double arm_range
	, float thickness, ICanvasBrush^ bcolor, ICanvasBrush^ acolor, ICanvasBrush^ sdcolor, ICanvasBrush^ ddcolor, ICanvasBrush^ hmcolor)
	: radius(std::fabsf(radius)), thickness(thickness), sign((radius < 0.0F) ? 1.0F : -1.0F), precision(2U)
	, depth_interval(10.0F), visor_range(visor_range), arm_range(arm_range), offset(30.0)
	, visor_color(Colours::make(color))
	, body_color(bcolor == nullptr ? drag_default_head_color : bcolor)
	, angle_pointer_color(acolor == nullptr ? drag_default_angle_pointer_color : acolor)
	, suction_pointer_color(acolor == nullptr ? drag_default_suction_depth_pointer_color : sdcolor)
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
	SET_BOXES(ts, bs, this->vspace);
}

void DragHeadlet::construct() {
	RHatchMarkMetrics vmetrics, ametrics;
	VHatchMarkMetrics dmetrics;
	float aradius = this->radius * 1.000F;
	float vradius = this->radius * 0.618F;
	unsigned int depth_step = ((unsigned int)std::round(this->depth_range / depth_interval)) + 1;
	double adeg0 = drag_adjusted_angle(0.0, this->sign);
	double adegn = drag_adjusted_angle(this->arm_range, this->sign);
	double vdeg0 = drag_adjusted_angle(this->offset, this->sign);
	double vdegn = drag_adjusted_angle(drag_visor_end_angle, this->sign);
	auto ahatchmark = rhatchmark(aradius, adeg0, adegn, 0.0, this->arm_range, 0U, this->thickness, &ametrics, 0U);
	auto vhatchmark = rhatchmark(vradius, vdeg0, vdegn, 0.0, this->visor_range, 0U, this->thickness, &vmetrics, 0U);
	float head_radius = vmetrics.ring_radius - vmetrics.ch * 0.618F;
	float arm_thickness = head_radius * 0.618F * 2.0F;
	
	this->vspace = this->radius + ametrics.label_ty;
	this->translate_x = this->radius - this->radius * 2.0F * 0.382F;
	this->translate_y = 0.0F;
	this->visor_radius = head_radius - (vmetrics.ring_radius - head_radius) * 0.618F;
	this->bottom_radius = head_radius * 0.618F;
	this->depth_font = make_bold_text_format(ametrics.em * 0.95F);

	this->arrow_radius = ametrics.ch * 0.5F;
	this->visor_pointer_radius = vmetrics.ring_radius - this->arrow_radius * 2.0F;
	this->arm_pointer_radius = ametrics.ring_radius - this->arrow_radius * 2.0F;
	
	this->draghead = geometry_freeze(make_draghead(head_radius, this->bottom_radius, arm_thickness, this->radius + this->translate_x,
		this->offset, 0.0, this->sign, this->thickness * 4.0F));

	if (this->sign > 0.0F) { // leftward
		float height = -ametrics.label_ty - this->bottom_radius + this->translate_y;
		auto dhatchmark = vrhatchmark(height, -this->depth_range, depth_interval, depth_step, 1.0F, &dmetrics, 0U, true, this->depth_font);
		float arrow_length = dmetrics.width;
		auto arrow = hline(arrow_length);
		auto arrowhead = polar_arrowhead(this->arrow_radius, 0.0);
		
		this->depth_x = dmetrics.width + dmetrics.gap_space;
		this->depth_pointer = geometry_freeze(geometry_translate(geometry_union(arrowhead, arrow, -arrow_length), -this->arrow_radius * 2.0F));
		this->hatchmarks = geometry_freeze(geometry_union(geometry_union(vhatchmark, ahatchmark), dhatchmark, 0.0F, this->bottom_radius));
	} else {
		float height = -ametrics.label_ty - this->bottom_radius + this->translate_y;
		auto dhatchmark = vlhatchmark(height, -this->depth_range, depth_interval, depth_step, 1.0F, &dmetrics, 0U, true, this->depth_font);
		auto arrow = hline(dmetrics.width);
		auto arrowhead = polar_arrowhead(this->arrow_radius, 180.0);
		
		this->depth_x = -dmetrics.width - dmetrics.gap_space;
		this->depth_pointer = geometry_freeze(geometry_translate(geometry_union(arrowhead, arrow), this->arrow_radius * 2.0F));
		this->hatchmarks = geometry_freeze(geometry_union(geometry_union(vhatchmark, ahatchmark), dhatchmark,
			-dmetrics.width, this->bottom_radius));
	}

	this->depth_top = this->bottom_radius + dmetrics.hatch_y;
	this->depth_height = dmetrics.hatch_height;

	this->set_angles(0.0, 0.0, true);
	this->set_depths(0.0F, 0.0F, true);
}

void DragHeadlet::set_angles(double visor_angle, double arm_angle, bool force) {
	float visor_length = this->radius - this->translate_x;
	double arm_degrees = drag_adjusted_angle(arm_angle, this->sign);
	double visor_pointer_range = drag_visor_end_angle - offset;
	double visor_degrees = drag_adjusted_angle(visor_angle / this->visor_range * visor_pointer_range + this->offset, this->sign);
	
	if (force || (this->visor_pointer_degrees != visor_degrees)) {
		this->visor_pointer_degrees = visor_degrees;
		this->visor = geometry_freeze(make_visor(this->visor_radius, this->bottom_radius, visor_length, visor_angle, 0.0F, this->sign));
		this->visor_pointer = geometry_freeze(make_pointer(this->visor_pointer_radius, visor_degrees, this->arrow_radius, this->pointer_style));
	}

	if (force || (this->arm_pointer_degrees != arm_degrees)) {
		this->arm_pointer_degrees = arm_degrees;
		this->arm_pointer = geometry_freeze(make_pointer(this->arm_pointer_radius, arm_degrees, this->arrow_radius, this->pointer_style));
	}
}

void DragHeadlet::set_depths(float suction_depth, float draghead_depth, bool force) {
	if (force || (this->suction_depth != -suction_depth)) {
		this->suction_depth = -suction_depth;
		this->suction_m = make_text_layout(flstring(suction_depth, this->precision), this->depth_font);
	}

	if (force || (this->draghead_depth != -draghead_depth)) {
		this->draghead_depth = -draghead_depth;
		this->depth_m = make_text_layout(flstring(draghead_depth, this->precision), this->depth_font);
	}
}

void DragHeadlet::draw(CanvasDrawingSession^ ds, float x, float y, float Width, float Height) {
	float cx = x + this->radius - this->translate_x * this->sign;
	float cy = y + this->radius - this->translate_y;

	ds->DrawCachedGeometry(this->visor, cx, cy, this->visor_color);
	ds->DrawCachedGeometry(this->draghead, cx, cy, this->body_color);
	ds->DrawCachedGeometry(this->hatchmarks, cx, cy, this->hatchmark_color);

	{ // draw pointers
		Rect suction_box = this->suction_m->LayoutBounds;
		Rect depth_box = this->depth_m->LayoutBounds;
		float depth_range = this->depth_interval + this->depth_range;
		float suction_dy = (this->depth_interval - this->suction_depth) / depth_range * this->depth_height + this->depth_top;
		float draghead_dy = (this->depth_interval - this->draghead_depth) / depth_range * this->depth_height + this->depth_top;
		float depth_x = cx + this->depth_x;
		float depth_meter_y = cy + draghead_dy - depth_box.Height * 0.5F;
		float suction_meter_y = cy + suction_dy - suction_box.Height * 0.5F;

		ds->DrawCachedGeometry(this->visor_pointer, cx, cy, this->angle_pointer_color);
		ds->DrawCachedGeometry(this->arm_pointer, cx, cy, this->angle_pointer_color);

		ds->DrawCachedGeometry(this->depth_pointer, cx, cy + draghead_dy, this->draghead_pointer_color);
		ds->DrawCachedGeometry(this->depth_pointer, cx, cy + suction_dy, this->suction_pointer_color);

		if (this->sign > 0.0F) { // leftward
			ds->DrawTextLayout(this->depth_m, depth_x, depth_meter_y, this->draghead_pointer_color);
			ds->DrawTextLayout(this->suction_m, depth_x, suction_meter_y, this->suction_pointer_color);
		} else {
			ds->DrawTextLayout(this->depth_m, depth_x - depth_box.Width, depth_meter_y, this->draghead_pointer_color);
			ds->DrawTextLayout(this->suction_m, depth_x - suction_box.Width, suction_meter_y, this->suction_pointer_color);
		}
	}
}
