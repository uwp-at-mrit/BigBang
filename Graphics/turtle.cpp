#pragma once

#include "turtle.hpp"

#include "box.hpp"
#include "shape.hpp"
#include "paint.hpp"
#include "geometry.hpp"

using namespace WarGrey::SCADA;

using namespace Microsoft::Graphics::Canvas;
using namespace Microsoft::Graphics::Canvas::Geometry;

using namespace Windows::Foundation::Numerics;

ITurtle::ITurtle(float xstepsize, float ystepsize, bool big_turn, unsigned int start_anchor, unsigned int _anchor)
	: x(0.0F), y(0.0F), xstepsize(xstepsize), ystepsize(ystepsize), _anchor(_anchor) {
	this->xradius = xstepsize;
	this->yradius = ystepsize;

	if (!big_turn) {
		this->xradius *= 0.5F;
		this->yradius *= 0.5F;
	}

	this->do_rebuild();
	this->do_anchor(start_anchor);
}

void ITurtle::clear() {
	this->anchors.clear();
	this->snapshot = nullptr;
	this->last_backtrace_anchor = this->_anchor;

	this->do_rebuild();
}

void ITurtle::wipe() {
	this->snapshot = nullptr;
	this->do_rebuild();
}

void ITurtle::fill_stepsize(float* xstep, float* ystep) {
	SET_BOX(xstep, this->xstepsize);
	SET_BOX(ystep, this->ystepsize);
}

void ITurtle::fill_anchor_location(unsigned int anchor, float* x, float* y) {
	auto node = this->anchors.find(anchor);

	if (node != this->anchors.end()) {
		std::complex<float> pt = node->second;

		SET_BOX(x, pt.real());
		SET_BOX(y, pt.imag());
	}
}

CanvasGeometry^ ITurtle::subtrack(unsigned int a1, unsigned int a2, float thickness, CanvasStrokeStyle^ style) {
	float poff = thickness * 0.5F;
	float sext = poff * 2.0F;
	float x1, y1, x2, y2, x, y, w, h;

	this->fill_anchor_location(a1, &x1, &y1);
	this->fill_anchor_location(a2, &x2, &y2);

	if (x1 < x2) {
		x = x1;
		w = x2 - x1;
	} else {
		x = x2;
		w = x1 - x2;
	}

	if (y1 < y2) {
		y = y1;
		h = y2 - y;
	} else {
		y = y2;
		h = y1 - y2;
	}

	// TODO: handle track connection smoothly 
	return geometry_intersect(this->snap_track(thickness, style), rectangle(x - poff, y - poff, w + sext, h + sext));
}

CanvasGeometry^ ITurtle::snap_track(float thickness, CanvasStrokeStyle^ style) {
	// WARNING: `CanvasGeometry::CreatePath` will close the track leaving it unavailable for future use.
	if ((this->snapshot == nullptr) || this->moved) {
		this->track->EndFigure(CanvasFigureLoop::Open);
		auto the_track = CanvasGeometry::CreatePath(this->track);
		
		if (this->snapshot == nullptr) {
			this->snapshot = the_track;
		} else {
			this->snapshot = geometry_union(this->snapshot, the_track);
		}

		this->do_rebuild();
	}

	return geometry_stroke(this->snapshot, thickness, style);
}

/*************************************************************************************************/
void ITurtle::move_to(unsigned int a_id) {
	this->fill_anchor_location(a_id, &this->x, &this->y);
	this->do_move(this->_anchor);
}

void ITurtle::jump_back(unsigned int a_id) {
	unsigned int target = ((a_id == this->_anchor) ? this->last_backtrace_anchor : a_id);

	if (target != this->_anchor) {
		this->fill_anchor_location(target, &this->x, &this->y);
		this->last_backtrace_anchor = target;
	} else {
		this->x = 0.0F;
		this->y = 0.0F;
	}

	this->do_jump(this->_anchor);
}

void ITurtle::jump_up(float step, unsigned int a_id) {
	this->y -= (this->ystepsize * step);
	this->do_jump(a_id);
}

void ITurtle::jump_right(float step, unsigned int a_id) {
	this->x += (this->xstepsize * step);
	this->do_jump(a_id);
}

void ITurtle::jump_down(float step, unsigned int a_id) {
	this->y += (this->ystepsize * step);
	this->do_jump(a_id);
}

void ITurtle::jump_left(float step, unsigned int a_id) {
	this->x -= (this->xstepsize * step);
	this->do_jump(a_id);
}

void ITurtle::jump_up_right(float step, unsigned int a_id) {
	this->x += (this->xstepsize * step);
	this->y -= (this->ystepsize * step);
	this->do_jump(a_id);
}

void ITurtle::jump_right_down(float step, unsigned int a_id) {
	this->x += (this->xstepsize * step);
	this->y += (this->ystepsize * step);
	this->do_jump(a_id);
}

void ITurtle::jump_down_left(float step, unsigned int a_id) {
	this->x -= (this->xstepsize * step);
	this->y += (this->ystepsize * step);
	this->do_jump(a_id);
}

void ITurtle::jump_left_up(float step, unsigned int a_id) {
	this->x -= (this->xstepsize * step);
	this->y -= (this->ystepsize * step);
	this->do_jump(a_id);
}

void ITurtle::move_up(float step, unsigned int a_id) {
	this->y -= (this->ystepsize * step);
	this->do_move(a_id);
}

void ITurtle::move_right(float step, unsigned int a_id) {
	this->x += (this->xstepsize * step);
	this->do_move(a_id);
}

void ITurtle::move_down(float step, unsigned int a_id) {
	this->y += (this->ystepsize * step);
	this->do_move(a_id);
}

void ITurtle::move_left(float step, unsigned int a_id) {
	this->x -= (this->xstepsize * step);
	this->do_move(a_id);
}

void ITurtle::move_up_right(float step, unsigned int a_id) {
	this->x += (this->xstepsize * step);
	this->y -= (this->ystepsize * step);
	this->do_move(a_id);
}

void ITurtle::move_right_down(float step, unsigned int a_id) {
	this->x += (this->xstepsize * step);
	this->y += (this->ystepsize * step);
	this->do_move(a_id);
}

void ITurtle::move_down_left(float step, unsigned int a_id) {
	this->x -= (this->xstepsize * step);
	this->y += (this->ystepsize * step);
	this->do_move(a_id);
}

void ITurtle::move_left_up(float step, unsigned int a_id) {
	this->x -= (this->xstepsize * step);
	this->y -= (this->ystepsize * step);
	this->do_move(a_id);
}

void ITurtle::turn_down_left(unsigned int a_id) {
	this->x -= this->xradius;
	this->y += this->yradius;
	this->do_clockwise_turn(a_id);
}

void ITurtle::turn_left_down(unsigned int a_id) {
	this->x -= this->xradius;
	this->y += this->yradius;
	this->do_counterclockwise_turn(a_id);
}

void ITurtle::turn_down_right(unsigned int a_id) {
	this->x += this->xradius;
	this->y += this->yradius;
	this->do_counterclockwise_turn(a_id);
}

void ITurtle::turn_right_down(unsigned int a_id) {
	this->x += this->xradius;
	this->y += this->yradius;
	this->do_clockwise_turn(a_id);
}

void ITurtle::turn_left_up(unsigned int a_id) {
	this->x -= this->xradius;
	this->y -= this->yradius;
	this->do_clockwise_turn(a_id);
}

void ITurtle::turn_up_left(unsigned int a_id) {
	this->x -= this->xradius;
	this->y -= this->yradius;
	this->do_counterclockwise_turn(a_id);
}

void ITurtle::turn_right_up(unsigned int a_id) {
	this->x += this->xradius;
	this->y -= this->yradius;
	this->do_counterclockwise_turn(a_id);
}

void ITurtle::turn_up_right(unsigned int a_id) {
	this->x += this->xradius;
	this->y -= this->yradius;
	this->do_clockwise_turn(a_id);
}

void ITurtle::turn_down_left_up(unsigned int a_id) {
	this->x -= this->xradius * 2.0F;
	this->do_clockwise_turn(a_id);
}

void ITurtle::turn_down_right_up(unsigned int a_id) {
	this->x += this->xradius * 2.0F;
	this->do_counterclockwise_turn(a_id);
}

void ITurtle::turn_up_left_down(unsigned int a_id) {
	this->x -= this->xradius * 2.0F;
	this->do_counterclockwise_turn(a_id);
}

void ITurtle::turn_up_right_down(unsigned int a_id) {
	this->x += this->xradius * 2.0F;
	this->do_clockwise_turn(a_id);
}

void ITurtle::turn_left_down_right(unsigned int a_id) {
	this->y += this->yradius * 2.0F;
	this->do_counterclockwise_turn(a_id);
}

void ITurtle::turn_left_up_right(unsigned int a_id) {
	this->y -= this->yradius * 2.0F;
	this->do_clockwise_turn(a_id);
}

void ITurtle::turn_right_down_left(unsigned int a_id) {
	this->y += this->yradius * 2.0F;
	this->do_clockwise_turn(a_id);
}

void ITurtle::turn_right_up_left(unsigned int a_id) {
	this->y -= this->yradius * 2.0F;
	this->do_counterclockwise_turn(a_id);
}

/*************************************************************************************************/
void ITurtle::do_rebuild() {
	auto shared_ds = CanvasDevice::GetSharedDevice();

	this->moved = false;
	this->track = ref new CanvasPathBuilder(shared_ds);
	this->track->BeginFigure(this->x, this->y);
}

void ITurtle::do_step(unsigned int a_id) {
	this->do_anchor(a_id);
	this->moved = true;
}

void ITurtle::do_anchor(unsigned int a_id) {
	if (a_id != this->_anchor) {
		std::complex<float> key_point(this->x, this->y);
		auto anchor = std::pair<unsigned int, std::complex<float>>(a_id, key_point);
		auto ret = this->anchors.insert(anchor);

		if (!ret.second) {
			this->anchors.erase(ret.first);
			this->anchors.insert(anchor);
		}

		if (a_id > this->_anchor) {
			this->last_backtrace_anchor = a_id;
		}
	}
}

void ITurtle::do_jump(unsigned int a_id) {
	this->do_anchor(a_id);
	this->track->EndFigure(CanvasFigureLoop::Open);
	this->track->BeginFigure(this->x, this->y);
}

void ITurtle::do_move(unsigned int a_id) {
	this->do_step(a_id);
	this->track->AddLine(this->x, this->y);
}

void ITurtle::do_clockwise_turn(unsigned int a_id) {
	this->do_step(a_id);
	this->track->AddArc(float2(this->x, this->y), this->xradius, this->yradius, 0.0F,
		CanvasSweepDirection::Clockwise, CanvasArcSize::Small);
}

void ITurtle::do_counterclockwise_turn(unsigned int a_id) {
	this->do_step(a_id);
	this->track->AddArc(float2(this->x, this->y), this->xradius, this->yradius, 0.0F,
		CanvasSweepDirection::CounterClockwise, CanvasArcSize::Small);
}
