#pragma once

#include <unordered_map>
#include <complex>

#include "object.hpp"

namespace WarGrey::SCADA {
	private class Turtle final : public WarGrey::SCADA::SharedObject {
	public:
		Turtle(float stepsize, bool big_turn = false, int start_anchor_id = -1);

	public:
		void clear();
		void fill_anchor_location(int id, float* x, float* y);
		Microsoft::Graphics::Canvas::Geometry::CanvasGeometry^ snap_track(float thickness = 1.0F,
			Microsoft::Graphics::Canvas::Geometry::CanvasStrokeStyle^ style = nullptr);

	public:
		Turtle* jump_up(int step = 1, int anchor_id = -1);
		Turtle* jump_right(int step = 1, int anchor_id = -1);
		Turtle* jump_down(int step = 1, int anchor_id = -1);
		Turtle* jump_left(int step = 1, int anchor_id = -1);

		Turtle* jump_up(float step, int anchor_id = -1);
		Turtle* jump_right(float step, int anchor_id = -1);
		Turtle* jump_down(float step, int anchor_id = -1);
		Turtle* jump_left(float step, int anchor_id = -1);

	public:
		Turtle* move_up(int step = 1, int anchor_id = -1);
		Turtle* move_right(int step = 1, int anchor_id = -1);
		Turtle* move_down(int step = 1, int anchor_id = -1);
		Turtle* move_left(int step = 1, int anchor_id = -1);

		Turtle* move_up(float step, int anchor_id = -1);
		Turtle* move_right(float step, int anchor_id = -1);
		Turtle* move_down(float step, int anchor_id = -1);
		Turtle* move_left(float step, int anchor_id = -1);

	public:
		Turtle* turn_down_left(int anchor_id = -1);
		Turtle* turn_left_down(int anchor_id = -1);
		Turtle* turn_down_right(int anchor_id = -1);
		Turtle* turn_right_down(int anchor_id = -1);
		Turtle* turn_up_left(int anchor_id = -1);
		Turtle* turn_left_up(int anchor_id = -1);
		Turtle* turn_up_right(int anchor_id = -1);
		Turtle* turn_right_up(int anchor_id = -1);

	public:
		Turtle* turn_down_left_up(int anchor_id = -1);
		Turtle* turn_down_right_up(int anchor_id = -1);
		Turtle* turn_up_left_down(int anchor_id = -1);
		Turtle* turn_up_right_down(int anchor_id = -1);
		Turtle* turn_left_down_right(int anchor_id = -1);
		Turtle* turn_left_up_right(int anchor_id = -1);
		Turtle* turn_right_down_left(int anchor_id = -1);
		Turtle* turn_right_up_left(int anchor_id = -1);

	private:
		~Turtle() noexcept {}

	private:
		void do_rebuild();
		void do_step(int id);
		void do_anchor(int id);
		WarGrey::SCADA::Turtle* do_jump(int id);
		WarGrey::SCADA::Turtle* do_move(int id);
		WarGrey::SCADA::Turtle* do_clockwise_turn(int id);
		WarGrey::SCADA::Turtle* do_counterclockwise_turn(int id);

	private:
		Microsoft::Graphics::Canvas::Geometry::CanvasPathBuilder^ track;
		float stepsize;
		float tradius;
		float x;
		float y;

	private:
		Microsoft::Graphics::Canvas::Geometry::CanvasGeometry^ snapshot;
		std::unordered_map<int, std::complex<float>> anchors;
		bool moved;
	};
}
