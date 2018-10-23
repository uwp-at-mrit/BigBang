#include "graphlet/device/draglet.hpp"

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

static CanvasGeometry^ make_draghead(float radius, float arm_length, double break_point, double degrees) {
	auto head = ref new CanvasPathBuilder(CanvasDevice::GetSharedDevice());
	auto center = circle(radius * 0.2718F);
	float bradius = radius * 0.618F;
	float arm_diffradians = degrees_to_radians(break_point);
	float radians = degrees_to_radians(degrees);
	float tstart_radians = degrees_to_radians(degrees - 100.0);
	float tend_radians = degrees_to_radians(degrees - break_point * 0.8F);
	float bstart_radians = degrees_to_radians(degrees + break_point);
	float bsweep_radians = degrees_to_radians(180.0 - break_point);
	float aradius = arm_length;
	float2 tstart, aterminator, bstart;

	head->SetFilledRegionDetermination(CanvasFilledRegionDetermination::Winding);

	circle_point(radius, tstart_radians, &tstart.x, &tstart.y);
	circle_point(aradius, radians, &aterminator.x, &aterminator.y);
	circle_point(bradius, bstart_radians, &bstart.x, &bstart.y);

	head->BeginFigure(tstart);
	head->AddArc(float2(0.0F, 0.0F), radius, radius, tstart_radians, tend_radians - tstart_radians);
	head->AddLine(aterminator);
	head->AddLine(bstart);
	head->AddArc(float2(0.0F, 0.0F), bradius, bradius, bstart_radians, bsweep_radians);
	head->AddLine(0.0F, 0.0F);
	head->AddLine(tstart);
	head->EndFigure(CanvasFigureLoop::Closed);

	return geometry_union(CanvasGeometry::CreatePath(head), center);
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

	{ // make dragfloat3 keypoint;
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
	auto ahatchmark = rhatchmark(this->radius * 0.90F, 0.0, -this->range, 0.0, this->range, 0U, this->thickness, &ametrics, this->precision);
	auto vhatchmark = rhatchmark(this->radius * 0.50F, -this->offset, -100.0, 0.0, this->range, 0U, this->thickness, &vmetrics, this->precision);

	this->hatchmarks = geometry_freeze(geometry_union(vhatchmark, ahatchmark));
	this->head_radius = vmetrics.ring_radius - vmetrics.ch * 0.618F;

	this->draghead = make_draghead(this->head_radius, this->radius, this->offset, 0.0);
}

void DragHeadlet::draw(CanvasDrawingSession^ ds, float x, float y, float Width, float Height) {
	float cx = x + this->radius;
	float cy = y + this->radius;
	
	ds->FillGeometry(this->draghead, cx, cy, this->body_color);
	ds->DrawCachedGeometry(this->hatchmarks, cx, cy, this->hatchmark_color);

	ds->DrawRectangle(x, y, this->radius * 2.0F, this->radius * 2.0F, Colours::Crimson);
}
