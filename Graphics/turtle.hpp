#pragma once

#include <complex>
#include <algorithm>
#include <unordered_map>

#include "box.hpp"
#include "object.hpp"
#include "geometry.hpp"

namespace WarGrey::SCADA {
	template<typename Anchor>
	private class Turtle final : public WarGrey::SCADA::SharedObject {
	public:
		Turtle(float stepsize, bool big_turn = false, Anchor start_id = Anchor::_) : stepsize(stepsize), x(0.0F), y(0.0F) {
			this->tradius = stepsize * (big_turn ? 1.0F : 0.5F);
			this->do_rebuild();
			this->do_anchor(start_id);
		}

	public:
		void clear() {
			this->anchors.clear();
			this->snapshot = nullptr;
			this->last_backtrace_anchor = Anchor::_;

			this->do_rebuild();
		}

		void wipe() {
			this->snapshot = nullptr;
			this->do_rebuild();
		}

		void fill_anchor_location(Anchor id, float* x, float* y) {
			auto node = this->anchors.find(id);

			if (node != this->anchors.end()) {
				std::complex<float> pt = node->second;

				SET_BOX(x, pt.real());
				SET_BOX(y, pt.imag());
			}
		}

		Microsoft::Graphics::Canvas::Geometry::CanvasGeometry^ snap_track(float thickness = 1.0F,
			Microsoft::Graphics::Canvas::Geometry::CanvasStrokeStyle^ style = nullptr) {
			// WARNING: `CanvasGeometry::CreatePath` will close the track leaving it unavailable for future use.

			if ((this->snapshot == nullptr) || this->moved) {
				this->track->EndFigure(Microsoft::Graphics::Canvas::Geometry::CanvasFigureLoop::Open);
				auto trackpath = Microsoft::Graphics::Canvas::Geometry::CanvasGeometry::CreatePath(this->track);
				auto trackline = geometry_stroke(trackpath, thickness, style);

				if (this->snapshot == nullptr) {
					this->snapshot = trackline;
				} else {
					this->snapshot = geometry_union(this->snapshot, trackline);
				}

				this->do_rebuild();
			}

			return this->snapshot;
		}

	public:
		WarGrey::SCADA::Turtle<Anchor>* jump_up(Anchor id) {
			return this->jump_up(1.0F, id);
		}

		WarGrey::SCADA::Turtle<Anchor>* jump_right(Anchor id) {
			return this->jump_right(1.0F, id);
		}

		WarGrey::SCADA::Turtle<Anchor>* jump_down(Anchor id) {
			return this->jump_down(1.0F, id);
		}

		WarGrey::SCADA::Turtle<Anchor>* jump_left(Anchor id) {
			return this->jump_left(1.0F, id);
		}

		WarGrey::SCADA::Turtle<Anchor>* jump_up_right(Anchor id) {
			return this->jump_up_right(1.0F, id);
		}

		WarGrey::SCADA::Turtle<Anchor>* jump_right_up(Anchor id) {
			return this->jump_up_right(1.0F, id);
		}

		WarGrey::SCADA::Turtle<Anchor>* jump_right_down(Anchor id) {
			return this->jump_right_down(1.0F, id);
		}

		WarGrey::SCADA::Turtle<Anchor>* jump_down_right(Anchor id) {
			return this->jump_right_down(1.0F, id);
		}

		WarGrey::SCADA::Turtle<Anchor>* jump_down_left(Anchor id) {
			return this->jump_down_left(1.0F, id);
		}

		WarGrey::SCADA::Turtle<Anchor>* jump_left_down(Anchor id) {
			return this->jump_down_left(1.0F, id);
		}

		WarGrey::SCADA::Turtle<Anchor>* jump_left_up(Anchor id) {
			return this->jump_left_up(1.0F, id);
		}

		WarGrey::SCADA::Turtle<Anchor>* jump_up_left(Anchor id) {
			return this->jump_left_up(1.0F, id);
		}

		WarGrey::SCADA::Turtle<Anchor>* jump_up(int step = 1, Anchor id = Anchor::_) {
			return this->jump_up(float(step), id);
		}

		WarGrey::SCADA::Turtle<Anchor>* jump_right(int step = 1, Anchor id = Anchor::_) {
			return this->jump_right(float(step), id);
		}

		WarGrey::SCADA::Turtle<Anchor>* jump_down(int step = 1, Anchor id = Anchor::_) {
			return this->jump_down(float(step), id);
		}

		WarGrey::SCADA::Turtle<Anchor>* jump_left(int step = 1, Anchor id = Anchor::_) {
			return this->jump_left(float(step), id);
		}

		WarGrey::SCADA::Turtle<Anchor>* jump_up_right(int step = 1, Anchor id = Anchor::_) {
			return this->jump_up_right(float(step), id);
		}

		WarGrey::SCADA::Turtle<Anchor>* jump_right_up(int step = 1, Anchor id = Anchor::_) {
			return this->jump_up_right(float(step), id);
		}

		WarGrey::SCADA::Turtle<Anchor>* jump_right_down(int step = 1, Anchor id = Anchor::_) {
			return this->jump_right_down(float(step), id);
		}

		WarGrey::SCADA::Turtle<Anchor>* jump_down_right(int step = 1, Anchor id = Anchor::_) {
			return this->jump_right_down(float(step), id);
		}

		WarGrey::SCADA::Turtle<Anchor>* jump_down_left(int step = 1, Anchor id = Anchor::_) {
			return this->jump_down_left(float(step), id);
		}

		WarGrey::SCADA::Turtle<Anchor>* jump_left_down(int step = 1, Anchor id = Anchor::_) {
			return this->jump_down_left(float(step), id);
		}

		WarGrey::SCADA::Turtle<Anchor>* jump_left_up(int step = 1, Anchor id = Anchor::_) {
			return this->jump_left_up(float(step), id);
		}

		WarGrey::SCADA::Turtle<Anchor>* jump_up_left(int step = 1, Anchor id = Anchor::_) {
			return this->jump_left_up(float(step), id);
		}

		WarGrey::SCADA::Turtle<Anchor>* jump_right_up(float step, Anchor id = Anchor::_) {
			return this->jump_up_right(step, id);
		}

		WarGrey::SCADA::Turtle<Anchor>* jump_down_right(float step, Anchor id = Anchor::_) {
			return this->jump_right_down(step, id);
		}

		WarGrey::SCADA::Turtle<Anchor>* jump_left_down(float step, Anchor id = Anchor::_) {
			return this->jump_down_left(step, id);
		}

		WarGrey::SCADA::Turtle<Anchor>* jump_up_left(float step, Anchor id = Anchor::_) {
			return this->jump_left_up(step, id);
		}

	public:
		WarGrey::SCADA::Turtle<Anchor>* move_up(Anchor id) {
			return this->move_up(1.0F, id);
		}

		WarGrey::SCADA::Turtle<Anchor>* move_right(Anchor id) {
			return this->move_right(1.0F, id);
		}

		WarGrey::SCADA::Turtle<Anchor>* move_down(Anchor id) {
			return this->move_down(1.0F, id);
		}

		WarGrey::SCADA::Turtle<Anchor>* move_left(Anchor id) {
			return this->move_left(1.0F, id);
		}

		WarGrey::SCADA::Turtle<Anchor>* move_up_right(Anchor id) {
			return this->move_up_right(1.0F, id);
		}

		WarGrey::SCADA::Turtle<Anchor>* move_right_up(Anchor id) {
			return this->move_up_right(1.0F, id);
		}

		WarGrey::SCADA::Turtle<Anchor>* move_right_down(Anchor id) {
			return this->move_right_down(1.0F, id);
		}

		WarGrey::SCADA::Turtle<Anchor>* move_down_right(Anchor id) {
			return this->move_right_down(1.0F, id);
		}

		WarGrey::SCADA::Turtle<Anchor>* move_down_left(Anchor id) {
			return this->move_down_left(1.0F, id);
		}

		WarGrey::SCADA::Turtle<Anchor>* move_left_down(Anchor id) {
			return this->move_down_left(1.0F, id);
		}

		WarGrey::SCADA::Turtle<Anchor>* move_left_up(Anchor id) {
			return this->move_left_up(1.0F, id);
		}

		WarGrey::SCADA::Turtle<Anchor>* move_up_left(Anchor id) {
			return this->move_left_up(1.0F, id);
		}

		WarGrey::SCADA::Turtle<Anchor>* move_up(int step = 1, Anchor id = Anchor::_) {
			return this->move_up(float(step), id);
		}

		WarGrey::SCADA::Turtle<Anchor>* move_right(int step = 1, Anchor id = Anchor::_) {
			return this->move_right(float(step), id);
		}

		WarGrey::SCADA::Turtle<Anchor>* move_down(int step = 1, Anchor id = Anchor::_) {
			return this->move_down(float(step), id);
		}

		WarGrey::SCADA::Turtle<Anchor>* move_left(int step = 1, Anchor id = Anchor::_) {
			return this->move_left(float(step), id);
		}

		WarGrey::SCADA::Turtle<Anchor>* move_up_right(int step = 1, Anchor id = Anchor::_) {
			return this->move_up_right(float(step), id);
		}

		WarGrey::SCADA::Turtle<Anchor>* move_right_up(int step = 1, Anchor id = Anchor::_) {
			return this->move_up_right(float(step), id);
		}

		WarGrey::SCADA::Turtle<Anchor>* move_right_down(int step = 1, Anchor id = Anchor::_) {
			return this->move_right_down(float(step), id);
		}

		WarGrey::SCADA::Turtle<Anchor>* move_down_right(int step = 1, Anchor id = Anchor::_) {
			return this->move_right_down(float(step), id);
		}

		WarGrey::SCADA::Turtle<Anchor>* move_down_left(int step = 1, Anchor id = Anchor::_) {
			return this->move_down_left(float(step), id);
		}

		WarGrey::SCADA::Turtle<Anchor>* move_left_down(int step = 1, Anchor id = Anchor::_) {
			return this->move_down_left(float(step), id);
		}

		WarGrey::SCADA::Turtle<Anchor>* move_left_up(int step = 1, Anchor id = Anchor::_) {
			return this->move_left_up(float(step), id);
		}

		WarGrey::SCADA::Turtle<Anchor>* move_up_left(int step = 1, Anchor id = Anchor::_) {
			return this->move_left_up(float(step), id);
		}

		WarGrey::SCADA::Turtle<Anchor>* move_right_up(float step, Anchor id = Anchor::_) {
			return this->move_up_right(step, id);
		}

		WarGrey::SCADA::Turtle<Anchor>* move_down_right(float step, Anchor id = Anchor::_) {
			return this->move_right_down(step, id);
		}

		WarGrey::SCADA::Turtle<Anchor>* move_left_down(float step, Anchor id = Anchor::_) {
			return this->move_down_left(step, id);
		}

		WarGrey::SCADA::Turtle<Anchor>* move_up_left(float step, Anchor id = Anchor::_) {
			return this->move_left_up(step, id);
		}

	public:
		WarGrey::SCADA::Turtle<Anchor>* jump_up(float step, Anchor id = Anchor::_) {
			this->y -= (this->stepsize * step);

			return this->do_jump(id);
		}

		WarGrey::SCADA::Turtle<Anchor>* jump_right(float step, Anchor id = Anchor::_) {
			this->x += (this->stepsize * step);

			return this->do_jump(id);
		}

		WarGrey::SCADA::Turtle<Anchor>* jump_down(float step, Anchor id = Anchor::_) {
			this->y += (this->stepsize * step);

			return this->do_jump(id);
		}

		WarGrey::SCADA::Turtle<Anchor>* jump_left(float step, Anchor id = Anchor::_) {
			this->x -= (this->stepsize * step);

			return this->do_jump(id);
		}


		WarGrey::SCADA::Turtle<Anchor>* jump_up_right(float step, Anchor id = Anchor::_) {
			float _ = (this->stepsize * step);

			this->x += _;
			this->y -= _;

			return this->do_jump(id);
		}

		WarGrey::SCADA::Turtle<Anchor>* jump_right_down(float step, Anchor id = Anchor::_) {
			float _ = (this->stepsize * step);

			this->x += _;
			this->y += _;

			return this->do_jump(id);
		}

		WarGrey::SCADA::Turtle<Anchor>* jump_down_left(float step, Anchor id = Anchor::_) {
			float _ = (this->stepsize * step);

			this->x -= _;
			this->y += _;

			return this->do_jump(id);
		}

		WarGrey::SCADA::Turtle<Anchor>* jump_left_up(float step, Anchor id = Anchor::_) {
			float _ = (this->stepsize * step);

			this->x -= _;
			this->y -= _;

			return this->do_jump(id);
		}

	public:
		WarGrey::SCADA::Turtle<Anchor>* move_up(float step, Anchor id = Anchor::_) {
			this->y -= (this->stepsize * step);

			return this->do_move(id);
		}

		WarGrey::SCADA::Turtle<Anchor>* move_right(float step, Anchor id = Anchor::_) {
			this->x += (this->stepsize * step);

			return this->do_move(id);
		}

		WarGrey::SCADA::Turtle<Anchor>* move_down(float step, Anchor id = Anchor::_) {
			this->y += (this->stepsize * step);

			return this->do_move(id);
		}

		WarGrey::SCADA::Turtle<Anchor>* move_left(float step, Anchor id = Anchor::_) {
			this->x -= (this->stepsize * step);

			return this->do_move(id);
		}

		WarGrey::SCADA::Turtle<Anchor>* move_up_right(float step, Anchor id = Anchor::_) {
			float _ = (this->stepsize * step);

			this->x += _;
			this->y -= _;

			return this->do_move(id);
		}

		WarGrey::SCADA::Turtle<Anchor>* move_right_down(float step, Anchor id = Anchor::_) {
			float _ = (this->stepsize * step);

			this->x += _;
			this->y += _;

			return this->do_move(id);
		}

		WarGrey::SCADA::Turtle<Anchor>* move_down_left(float step, Anchor id = Anchor::_) {
			float _ = (this->stepsize * step);

			this->x -= _;
			this->y += _;

			return this->do_move(id);
		}

		WarGrey::SCADA::Turtle<Anchor>* move_left_up(float step, Anchor id = Anchor::_) {
			float _ = (this->stepsize * step);

			this->x -= _;
			this->y -= _;

			return this->do_move(id);
		}

	public:
		WarGrey::SCADA::Turtle<Anchor>* turn_down_left(Anchor id = Anchor::_) {
			this->x -= this->tradius;
			this->y += this->tradius;

			return this->do_clockwise_turn(id);
		}

		WarGrey::SCADA::Turtle<Anchor>* turn_left_down(Anchor id = Anchor::_) {
			this->x -= this->tradius;
			this->y += this->tradius;

			return this->do_counterclockwise_turn(id);
		}

		WarGrey::SCADA::Turtle<Anchor>* turn_down_right(Anchor id = Anchor::_) {
			this->x += this->tradius;
			this->y += this->tradius;

			return this->do_counterclockwise_turn(id);
		}

		WarGrey::SCADA::Turtle<Anchor>* turn_right_down(Anchor id = Anchor::_) {
			this->x += this->tradius;
			this->y += this->tradius;

			return this->do_clockwise_turn(id);
		}

		WarGrey::SCADA::Turtle<Anchor>* turn_left_up(Anchor id = Anchor::_) {
			this->x -= this->tradius;
			this->y -= this->tradius;

			return this->do_clockwise_turn(id);
		}

		WarGrey::SCADA::Turtle<Anchor>* turn_up_left(Anchor id = Anchor::_) {
			this->x -= this->tradius;
			this->y -= this->tradius;

			return this->do_counterclockwise_turn(id);
		}

		WarGrey::SCADA::Turtle<Anchor>* turn_right_up(Anchor id = Anchor::_) {
			this->x += this->tradius;
			this->y -= this->tradius;

			return this->do_counterclockwise_turn(id);
		}

		WarGrey::SCADA::Turtle<Anchor>* turn_up_right(Anchor id = Anchor::_) {
			this->x += this->tradius;
			this->y -= this->tradius;

			return this->do_clockwise_turn(id);
		}

	public:
		WarGrey::SCADA::Turtle<Anchor>* turn_down_left_up(Anchor id = Anchor::_) {
			this->x -= this->stepsize;

			return this->do_clockwise_turn(id);
		}

		WarGrey::SCADA::Turtle<Anchor>* turn_down_right_up(Anchor id = Anchor::_) {
			this->x += this->stepsize;

			return this->do_counterclockwise_turn(id);
		}

		WarGrey::SCADA::Turtle<Anchor>* turn_up_left_down(Anchor id = Anchor::_) {
			this->x -= this->stepsize;

			return this->do_counterclockwise_turn(id);
		}

		WarGrey::SCADA::Turtle<Anchor>* turn_up_right_down(Anchor id = Anchor::_) {
			this->x += this->stepsize;

			return this->do_clockwise_turn(id);
		}

		WarGrey::SCADA::Turtle<Anchor>* turn_left_down_right(Anchor id = Anchor::_) {
			this->y += this->stepsize;

			return this->do_counterclockwise_turn(id);
		}

		WarGrey::SCADA::Turtle<Anchor>* turn_left_up_right(Anchor id = Anchor::_) {
			this->y -= this->stepsize;

			return this->do_clockwise_turn(id);
		}

		WarGrey::SCADA::Turtle<Anchor>* turn_right_down_left(Anchor id = Anchor::_) {
			this->y += this->stepsize;

			return this->do_clockwise_turn(id);
		}

		WarGrey::SCADA::Turtle<Anchor>* turn_right_up_left(Anchor id = Anchor::_) {
			this->y -= this->stepsize;

			return this->do_counterclockwise_turn(id);
		}

	public:
		WarGrey::SCADA::Turtle<Anchor>* jump_back(Anchor id = Anchor::_) {
			Anchor target = ((id == Anchor::_) ? this->last_backtrace_anchor : id);

			if (target != Anchor::_) {
				this->fill_anchor_location(target, &this->x, &this->y);
				this->last_backtrace_anchor = target;
			} else {
				this->x = 0.0F;
				this->y = 0.0F;
			}

			return this->do_jump(Anchor::_);
		}

	private:
		~Turtle() noexcept {}

	private:
		void do_rebuild() {
			auto shared_ds = Microsoft::Graphics::Canvas::CanvasDevice::GetSharedDevice();

			this->moved = false;
			this->track = ref new Microsoft::Graphics::Canvas::Geometry::CanvasPathBuilder(shared_ds);
			this->track->BeginFigure(this->x, this->y);
		}

		void do_step(Anchor id) {
			this->do_anchor(id);
			this->moved = true;
		}

		void Turtle::do_anchor(Anchor id) {
			if (id != Anchor::_) {
				std::complex<float> key_point(this->x, this->y);
				auto anchor = std::pair<Anchor, std::complex<float>>(id, key_point);
				auto ret = this->anchors.insert(anchor);

				if (!ret.second) {
					this->anchors.erase(ret.first);
					this->anchors.insert(anchor);
				}

				if (id > Anchor::_) {
					this->last_backtrace_anchor = id;
				}
			}
		}

		WarGrey::SCADA::Turtle<Anchor>* do_jump(Anchor id) {
			this->do_anchor(id);
			this->track->EndFigure(Microsoft::Graphics::Canvas::Geometry::CanvasFigureLoop::Open);
			this->track->BeginFigure(this->x, this->y);

			return this;
		}

		WarGrey::SCADA::Turtle<Anchor>* do_move(Anchor id) {
			this->do_step(id);
			this->track->AddLine(this->x, this->y);

			return this;
		}

		WarGrey::SCADA::Turtle<Anchor>* do_clockwise_turn(Anchor id) {
			this->do_step(id);
			this->track->AddArc(Windows::Foundation::Numerics::float2(this->x, this->y),
				this->tradius, this->tradius, 0.0F,
				Microsoft::Graphics::Canvas::Geometry::CanvasSweepDirection::Clockwise,
				Microsoft::Graphics::Canvas::Geometry::CanvasArcSize::Small);

			return this;
		}

		WarGrey::SCADA::Turtle<Anchor>* do_counterclockwise_turn(Anchor id) {
			this->do_step(id);
			this->track->AddArc(Windows::Foundation::Numerics::float2(this->x, this->y),
				this->tradius, this->tradius, 0.0F,
				Microsoft::Graphics::Canvas::Geometry::CanvasSweepDirection::CounterClockwise,
				Microsoft::Graphics::Canvas::Geometry::CanvasArcSize::Small);

			return this;
		}

	private:
		Microsoft::Graphics::Canvas::Geometry::CanvasPathBuilder^ track;
		float stepsize;
		float tradius;
		float x;
		float y;

	private:
		Microsoft::Graphics::Canvas::Geometry::CanvasGeometry^ snapshot;
		std::unordered_map<Anchor, std::complex<float>> anchors;
		Anchor last_backtrace_anchor = Anchor::_;
		bool moved;
	};
}
