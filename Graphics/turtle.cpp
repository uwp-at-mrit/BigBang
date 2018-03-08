#include "turtle.hpp"
#include "geometry.hpp"

using namespace WarGrey::SCADA;

using namespace Microsoft::Graphics::Canvas;
using namespace Microsoft::Graphics::Canvas::Geometry;

using namespace Windows::Foundation::Numerics;

#define do_move() { \
    this->moved = true; \
    this->track->AddLine(this->x, this->y); \
    return this; \
}

#define do_clockwise_turn() { \
    this->moved = true; \
    this->track->AddArc(float2(this->x, this->y), this->tradius, this->tradius, 0.0F, CanvasSweepDirection::Clockwise, CanvasArcSize::Small); \
    return this; \
}

#define do_counterclockwise_turn() { \
    this->moved = true; \
	this->track->AddArc(float2(this->x, this->y), this->tradius, this->tradius, 0.0F, CanvasSweepDirection::CounterClockwise, CanvasArcSize::Small); \
    return this; \
}

/*************************************************************************************************/
Turtle::Turtle(float stepsize) : stepsize(stepsize), tradius(stepsize * 0.5F), x(0.0F), y(0.0F) {
	this->clear();
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

		this->clear();
	}

	return this->snapshot;
}

void Turtle::clear() {
	this->track = ref new CanvasPathBuilder(CanvasDevice::GetSharedDevice());
	this->track->BeginFigure(this->x, this->y);
	this->moved = false;
}

Turtle* Turtle::move_up(int step)    { return this->move_up(float(step)); }
Turtle* Turtle::move_right(int step) { return this->move_right(float(step)); }
Turtle* Turtle::move_down(int step)  { return this->move_down(float(step)); }
Turtle* Turtle::move_left(int step)  { return this->move_left(float(step)); }

Turtle* Turtle::move_up(float step)    { this->y -= (this->stepsize * step); do_move(); }
Turtle* Turtle::move_right(float step) { this->x += (this->stepsize * step); do_move(); }
Turtle* Turtle::move_down(float step)  { this->y += (this->stepsize * step); do_move(); }
Turtle* Turtle::move_left(float step)  { this->x -= (this->stepsize * step); do_move(); }

Turtle* Turtle::turn_down_left()  { this->x -= this->tradius; this->y += this->tradius; do_clockwise_turn(); }
Turtle* Turtle::turn_left_down()  { this->x -= this->tradius; this->y += this->tradius; do_counterclockwise_turn(); }
Turtle* Turtle::turn_down_right() { this->x += this->tradius; this->y += this->tradius; do_counterclockwise_turn(); }
Turtle* Turtle::turn_right_down() { this->x += this->tradius; this->y += this->tradius; do_clockwise_turn(); }
Turtle* Turtle::turn_left_up()    { this->x -= this->tradius; this->y -= this->tradius; do_clockwise_turn(); }
Turtle* Turtle::turn_up_left()    { this->x -= this->tradius; this->y -= this->tradius; do_counterclockwise_turn(); }
Turtle* Turtle::turn_right_up()   { this->x += this->tradius; this->y -= this->tradius; do_counterclockwise_turn(); }
Turtle* Turtle::turn_up_right()   { this->x += this->tradius; this->y -= this->tradius; do_clockwise_turn(); }

Turtle* Turtle::turn_down_left_up()    { this->x -= this->stepsize; do_clockwise_turn(); }
Turtle* Turtle::turn_down_right_up()   { this->x += this->stepsize; do_counterclockwise_turn(); }
Turtle* Turtle::turn_up_left_down()    { this->x -= this->stepsize; do_counterclockwise_turn(); }
Turtle* Turtle::turn_up_right_down()   { this->x += this->stepsize; do_clockwise_turn(); }
Turtle* Turtle::turn_left_down_right() { this->y += this->stepsize; do_counterclockwise_turn(); }
Turtle* Turtle::turn_left_up_right()   { this->y -= this->stepsize; do_clockwise_turn(); }
Turtle* Turtle::turn_right_down_left() { this->y += this->stepsize; do_clockwise_turn(); }
Turtle* Turtle::turn_right_up_left()   { this->y -= this->stepsize; do_counterclockwise_turn(); }
