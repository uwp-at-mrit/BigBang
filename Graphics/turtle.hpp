#pragma once

#include "object.hpp"

namespace WarGrey::SCADA {
	private class Turtle final : public WarGrey::SCADA::SharedObject {
	public:
		Turtle(float stepsize);

	public:
		void clear();
		Microsoft::Graphics::Canvas::Geometry::CanvasGeometry^ snap_track(float thickness = 1.0F,
			Microsoft::Graphics::Canvas::Geometry::CanvasStrokeStyle^ style = nullptr);

	public:
		Turtle* move_up(int step = 1);
		Turtle* move_right(int step = 1);
		Turtle* move_down(int step = 1);
		Turtle* move_left(int step = 1);

		Turtle* move_up(float step);
		Turtle* move_right(float step);
		Turtle* move_down(float step);
		Turtle* move_left(float step);

	public:
		Turtle* turn_down_left();
		Turtle* turn_left_down();
		Turtle* turn_down_right();
		Turtle* turn_right_down();
		Turtle* turn_up_left();
		Turtle* turn_left_up();
		Turtle* turn_up_right();
		Turtle* turn_right_up();

	public:
		Turtle* turn_down_left_up();
		Turtle* turn_down_right_up();
		Turtle* turn_up_left_down();
		Turtle* turn_up_right_down();
		Turtle* turn_left_down_right();
		Turtle* turn_left_up_right();
		Turtle* turn_right_down_left();
		Turtle* turn_right_up_left();

	private:
		~Turtle() noexcept {}

	private:
		Microsoft::Graphics::Canvas::Geometry::CanvasPathBuilder^ track;
		float stepsize;
		float tradius;
		float x;
		float y;

	private:
		Microsoft::Graphics::Canvas::Geometry::CanvasGeometry^ snapshot;
		bool moved;
	};
}
