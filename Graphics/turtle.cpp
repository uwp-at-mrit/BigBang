#include <algorithm>

#include "box.hpp"
#include "turtle.hpp"
#include "geometry.hpp"

using namespace WarGrey::SCADA;

using namespace Microsoft::Graphics::Canvas;
using namespace Microsoft::Graphics::Canvas::Geometry;

using namespace Windows::Foundation;
using namespace Windows::Foundation::Numerics;

/*************************************************************************************************/
Turtle::Turtle(float stepsize, bool big_turn, int start_anchor)
	: stepsize(stepsize), tradius(stepsize * (big_turn ? 1.0F : 0.5F)), x(0.0F), y(0.0F) {
	this->do_rebuild();
	this->do_anchor(start_anchor);
}

CanvasGeometry^ Turtle::snap_track(float thickness, CanvasStrokeStyle^ style) {
	// WARNING: `CanvasGeometry::CreatePath` will close the track leaving it unavailable for future use.
	
	if ((this->snapshot == nullptr) || this->moved) {
		this->track->EndFigure(CanvasFigureLoop::Open);
		auto trackline = geometry_stroke(CanvasGeometry::CreatePath(this->track), thickness, style);

		if (this->snapshot == nullptr) {
			this->snapshot = trackline;
		} else {
			this->snapshot = geometry_union(this->snapshot, trackline);
		}

		this->do_rebuild();
	}

	return this->snapshot;
}

void Turtle::clear() {
	this->anchors.clear();
	this->snapshot = nullptr;

	this->do_rebuild();
}

void Turtle::fill_anchor_location(int id, float* x, float* y) {
	auto node = this->anchors.find(id);

	if (node != this->anchors.end()) {
		std::complex<float> pt = node->second;

		SET_BOX(x, pt.real());
		SET_BOX(y, pt.imag());
	}
}

Turtle* Turtle::jump_up(int step, int id) { return this->jump_up(float(step), id); }
Turtle* Turtle::jump_right(int step, int id) { return this->jump_right(float(step), id); }
Turtle* Turtle::jump_down(int step, int id) { return this->jump_down(float(step), id); }
Turtle* Turtle::jump_left(int step, int id) { return this->jump_left(float(step), id); }

Turtle* Turtle::jump_up(float step, int id) { this->y -= (this->stepsize * step); return do_jump(id); }
Turtle* Turtle::jump_right(float step, int id) { this->x += (this->stepsize * step); return do_jump(id); }
Turtle* Turtle::jump_down(float step, int id) { this->y += (this->stepsize * step); return do_jump(id); }
Turtle* Turtle::jump_left(float step, int id) { this->x -= (this->stepsize * step); return do_jump(id); }

Turtle* Turtle::move_up(int step, int id)    { return this->move_up(float(step), id); }
Turtle* Turtle::move_right(int step, int id) { return this->move_right(float(step), id); }
Turtle* Turtle::move_down(int step, int id)  { return this->move_down(float(step), id); }
Turtle* Turtle::move_left(int step, int id)  { return this->move_left(float(step), id); }

Turtle* Turtle::move_up(float step, int id)    { this->y -= (this->stepsize * step); return do_move(id); }
Turtle* Turtle::move_right(float step, int id) { this->x += (this->stepsize * step); return do_move(id); }
Turtle* Turtle::move_down(float step, int id)  { this->y += (this->stepsize * step); return do_move(id); }
Turtle* Turtle::move_left(float step, int id)  { this->x -= (this->stepsize * step); return do_move(id); }

Turtle* Turtle::turn_down_left(int id)  { this->x -= this->tradius; this->y += this->tradius; return this->do_clockwise_turn(id); }
Turtle* Turtle::turn_left_down(int id)  { this->x -= this->tradius; this->y += this->tradius; return this->do_counterclockwise_turn(id); }
Turtle* Turtle::turn_down_right(int id) { this->x += this->tradius; this->y += this->tradius; return this->do_counterclockwise_turn(id); }
Turtle* Turtle::turn_right_down(int id) { this->x += this->tradius; this->y += this->tradius; return this->do_clockwise_turn(id); }
Turtle* Turtle::turn_left_up(int id)    { this->x -= this->tradius; this->y -= this->tradius; return this->do_clockwise_turn(id); }
Turtle* Turtle::turn_up_left(int id)    { this->x -= this->tradius; this->y -= this->tradius; return this->do_counterclockwise_turn(id); }
Turtle* Turtle::turn_right_up(int id)   { this->x += this->tradius; this->y -= this->tradius; return this->do_counterclockwise_turn(id); }
Turtle* Turtle::turn_up_right(int id)   { this->x += this->tradius; this->y -= this->tradius; return this->do_clockwise_turn(id); }

Turtle* Turtle::turn_down_left_up(int id)    { this->x -= this->stepsize; return this->do_clockwise_turn(id); }
Turtle* Turtle::turn_down_right_up(int id)   { this->x += this->stepsize; return this->do_counterclockwise_turn(id); }
Turtle* Turtle::turn_up_left_down(int id)    { this->x -= this->stepsize; return this->do_counterclockwise_turn(id); }
Turtle* Turtle::turn_up_right_down(int id)   { this->x += this->stepsize; return this->do_clockwise_turn(id); }
Turtle* Turtle::turn_left_down_right(int id) { this->y += this->stepsize; return this->do_counterclockwise_turn(id); }
Turtle* Turtle::turn_left_up_right(int id)   { this->y -= this->stepsize; return this->do_clockwise_turn(id); }
Turtle* Turtle::turn_right_down_left(int id) { this->y += this->stepsize; return this->do_clockwise_turn(id); }
Turtle* Turtle::turn_right_up_left(int id)   { this->y -= this->stepsize; return this->do_counterclockwise_turn(id); }

void Turtle::do_rebuild() {
	this->moved = false;

	this->track = ref new CanvasPathBuilder(CanvasDevice::GetSharedDevice());
	this->track->BeginFigure(this->x, this->y);
}

void Turtle::do_step(int id) {
	this->do_anchor(id);
	this->moved = true;
}

void Turtle::do_anchor(int id) {
	if (id >= 0) {
		std::complex<float> key_point(this->x, this->y);
		auto anchor = std::pair<int, std::complex<float>>(id, key_point);
		this->anchors.insert(anchor);
	}
}

Turtle* Turtle::do_jump(int id) {
	this->do_anchor(id);
	this->track->EndFigure(CanvasFigureLoop::Open);
	this->track->BeginFigure(this->x, this->y);

	return this;
}

Turtle* Turtle::do_move(int id) {
    this->do_step(id);
    this->track->AddLine(this->x, this->y);

    return this;
}

Turtle* Turtle::do_clockwise_turn(int id) {
	this->do_step(id);
    this->track->AddArc(float2(this->x, this->y),
		this->tradius, this->tradius, 0.0F,
		CanvasSweepDirection::Clockwise, CanvasArcSize::Small);
    
	return this;
}

Turtle* Turtle::do_counterclockwise_turn(int id) {
	this->do_step(id);
	this->track->AddArc(float2(this->x, this->y),
		this->tradius, this->tradius, 0.0F,
		CanvasSweepDirection::CounterClockwise, CanvasArcSize::Small);

	return this;
}
