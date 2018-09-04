#pragma once

#include <complex>
#include <unordered_map>

#include "enum.hpp"
#include "shape.hpp"
#include "object.hpp"

namespace WarGrey::SCADA {
	private class ITurtle abstract : public WarGrey::SCADA::SharedObject {
	public:
		void clear();
		void wipe();
		Microsoft::Graphics::Canvas::Geometry::CanvasGeometry^ snap_track(float thickness = 1.0F,
			Microsoft::Graphics::Canvas::Geometry::CanvasStrokeStyle^ style = nullptr);

	protected:
		virtual ~ITurtle() noexcept {}
		ITurtle(float xstepsize, float ystepsize, bool big_turn, unsigned int start_anchor, unsigned int _anchor);

	protected:
		void fill_anchor_location(unsigned int anchor, float* x, float* y);
		Microsoft::Graphics::Canvas::Geometry::CanvasGeometry^ subtrack(unsigned int a1, unsigned int a2,
			float thickness, Microsoft::Graphics::Canvas::Geometry::CanvasStrokeStyle^ style);

	protected:
		void jump_back(unsigned int a_id);
		void jump_up(float step, unsigned int a_id);
		void jump_right(float step, unsigned int a_id);
		void jump_down(float step, unsigned int a_id);
		void jump_left(float step, unsigned int a_id);
		void jump_up_right(float step, unsigned int a_id);
		void jump_right_down(float step, unsigned int a_id);
		void jump_down_left(float step, unsigned int a_id);
		void jump_left_up(float step, unsigned int a_id);

	protected:
		void move_up(float step, unsigned int a_id);
		void move_right(float step, unsigned int a_id);
		void move_down(float step, unsigned int a_id);
		void move_left(float step, unsigned int a_id);
		void move_up_right(float step, unsigned int a_id);
		void move_right_down(float step, unsigned int a_id);
		void move_down_left(float step, unsigned int a_id);
		void move_left_up(float step, unsigned int a_id);

	protected:
		void turn_down_left(unsigned int a_id);
		void turn_left_down(unsigned int a_id);
		void turn_down_right(unsigned int a_id);
		void turn_right_down(unsigned int a_id);
		void turn_left_up(unsigned int a_id);
		void turn_up_left(unsigned int a_id);
		void turn_right_up(unsigned int a_id);
		void turn_up_right(unsigned int a_id);

	protected:
		void turn_down_left_up(unsigned int a_id);
		void turn_down_right_up(unsigned int a_id);
		void turn_up_left_down(unsigned int a_id);
		void turn_up_right_down(unsigned int a_id);
		void turn_left_down_right(unsigned int a_id);
		void turn_left_up_right(unsigned int a_id);
		void turn_right_down_left(unsigned int a_id);
		void turn_right_up_left(unsigned int a_id);

	private:
		void do_rebuild();
		void do_step(unsigned int a_id);
		void do_anchor(unsigned int a_id);
		void do_jump(unsigned int a_id);
		void do_move(unsigned int a_id);
		void do_clockwise_turn(unsigned int a_id);
		void do_counterclockwise_turn(unsigned int a_id);

	private:
		Microsoft::Graphics::Canvas::Geometry::CanvasPathBuilder^ track;
		float xstepsize;
		float ystepsize;
		float xradius;
		float yradius;
		float x;
		float y;

	private:
		Microsoft::Graphics::Canvas::Geometry::CanvasGeometry^ snapshot;
		std::unordered_map<unsigned int, std::complex<float>> anchors;
		unsigned int last_backtrace_anchor;
		unsigned int _anchor;
		bool moved;
	}; 
	
	template<typename Anchor>
	private class Turtle final : public WarGrey::SCADA::ITurtle {
	public:
		Turtle(float xstepsize, float ystepsize, bool big_turn = false, Anchor start_anchor = Anchor::_)
			: WarGrey::SCADA::ITurtle(xstepsize, ystepsize, big_turn, _I(start_anchor), _I(Anchor::_)) {}

		Turtle(float stepsize, bool big_turn = false, Anchor start_anchor = Anchor::_)
			: WarGrey::SCADA::Turtle<Anchor>(stepsize, stepsize, big_turn, start_anchor) {}

	public:
		void fill_anchor_location(Anchor a, float* x, float* y) {
			WarGrey::SCADA::ITurtle::fill_anchor_location(_I(a), x, y);
		}

		Microsoft::Graphics::Canvas::Geometry::CanvasGeometry^ subtrack(Anchor a1, Anchor a2
			, float thickness = 1.0F, Microsoft::Graphics::Canvas::Geometry::CanvasStrokeStyle^ style = nullptr) {
			return WarGrey::SCADA::ITurtle::subtrack(_I(a1), _I(a2), thickness, style);
		}

		Microsoft::Graphics::Canvas::Geometry::CanvasGeometry^ subtrack(Anchor as[], unsigned int count
			, float thickness = 1.0F, Microsoft::Graphics::Canvas::Geometry::CanvasStrokeStyle^ style = nullptr) {
			Microsoft::Graphics::Canvas::Geometry::CanvasGeometry^ track = WarGrey::SCADA::blank();

			for (unsigned int idx = 0; idx < count - 1; idx++) {
				track = WarGrey::SCADA::geometry_union(track,
					WarGrey::SCADA::ITurtle::subtrack(_I(as[idx]), _I(as[idx + 1]), thickness, style));
			}

			return track;
		}

		template<size_t N>
		Microsoft::Graphics::Canvas::Geometry::CanvasGeometry^ subtrack(Anchor (&as)[N]
			, float thickness = 1.0F, Microsoft::Graphics::Canvas::Geometry::CanvasStrokeStyle^ style = nullptr) {
			return this->subtrack(as, N, thickness, style);
		}

		WarGrey::SCADA::Turtle<Anchor>* jump_back(Anchor id = Anchor::_) {
			WarGrey::SCADA::ITurtle::jump_back(_I(id));
			return this;
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
			WarGrey::SCADA::ITurtle::jump_up(step, _I(id));
			return this;
		}

		WarGrey::SCADA::Turtle<Anchor>* jump_right(float step, Anchor id = Anchor::_) {
			WarGrey::SCADA::ITurtle::jump_right(step, _I(id));
			return this;
		}

		WarGrey::SCADA::Turtle<Anchor>* jump_down(float step, Anchor id = Anchor::_) {
			WarGrey::SCADA::ITurtle::jump_down(step, _I(id));
			return this;
		}

		WarGrey::SCADA::Turtle<Anchor>* jump_left(float step, Anchor id = Anchor::_) {
			WarGrey::SCADA::ITurtle::jump_left(step, _I(id));
			return this;
		}


		WarGrey::SCADA::Turtle<Anchor>* jump_up_right(float step, Anchor id = Anchor::_) {
			WarGrey::SCADA::ITurtle::jump_up_right(step, _I(id));
			return this;
		}

		WarGrey::SCADA::Turtle<Anchor>* jump_right_down(float step, Anchor id = Anchor::_) {
			WarGrey::SCADA::ITurtle::jump_right_down(step, _I(id));
			return this;
		}

		WarGrey::SCADA::Turtle<Anchor>* jump_down_left(float step, Anchor id = Anchor::_) {
			WarGrey::SCADA::ITurtle::jump_down_left(step, _I(id));
			return this;
		}

		WarGrey::SCADA::Turtle<Anchor>* jump_left_up(float step, Anchor id = Anchor::_) {
			WarGrey::SCADA::ITurtle::jump_left_up(step, _I(id));
			return this;
		}

	public:
		WarGrey::SCADA::Turtle<Anchor>* move_up(float step, Anchor id = Anchor::_) {
			WarGrey::SCADA::ITurtle::move_up(step, _I(id));
			return this;
		}

		WarGrey::SCADA::Turtle<Anchor>* move_right(float step, Anchor id = Anchor::_) {
			WarGrey::SCADA::ITurtle::move_right(step, _I(id));
			return this;
		}

		WarGrey::SCADA::Turtle<Anchor>* move_down(float step, Anchor id = Anchor::_) {
			WarGrey::SCADA::ITurtle::move_down(step, _I(id));
			return this;
		}

		WarGrey::SCADA::Turtle<Anchor>* move_left(float step, Anchor id = Anchor::_) {
			WarGrey::SCADA::ITurtle::move_left(step, _I(id));
			return this;
		}

		WarGrey::SCADA::Turtle<Anchor>* move_up_right(float step, Anchor id = Anchor::_) {
			WarGrey::SCADA::ITurtle::move_up_right(step, _I(id));
			return this;
		}

		WarGrey::SCADA::Turtle<Anchor>* move_right_down(float step, Anchor id = Anchor::_) {
			WarGrey::SCADA::ITurtle::move_right_down(step, _I(id));
			return this;
		}

		WarGrey::SCADA::Turtle<Anchor>* move_down_left(float step, Anchor id = Anchor::_) {
			WarGrey::SCADA::ITurtle::move_down_left(step, _I(id));
			return this;
		}

		WarGrey::SCADA::Turtle<Anchor>* move_left_up(float step, Anchor id = Anchor::_) {
			WarGrey::SCADA::ITurtle::move_left_up(step, _I(id));
			return this;
		}

	public:
		WarGrey::SCADA::Turtle<Anchor>* turn_down_left(Anchor id = Anchor::_) {
			WarGrey::SCADA::ITurtle::turn_down_left(_I(id));
			return this;
		}

		WarGrey::SCADA::Turtle<Anchor>* turn_left_down(Anchor id = Anchor::_) {
			WarGrey::SCADA::ITurtle::turn_left_down(_I(id));
			return this;
		}

		WarGrey::SCADA::Turtle<Anchor>* turn_down_right(Anchor id = Anchor::_) {
			WarGrey::SCADA::ITurtle::turn_down_right(_I(id));
			return this;
		}

		WarGrey::SCADA::Turtle<Anchor>* turn_right_down(Anchor id = Anchor::_) {
			WarGrey::SCADA::ITurtle::turn_right_down(_I(id));
			return this;
		}

		WarGrey::SCADA::Turtle<Anchor>* turn_left_up(Anchor id = Anchor::_) {
			WarGrey::SCADA::ITurtle::turn_left_up(_I(id));
			return this;
		}

		WarGrey::SCADA::Turtle<Anchor>* turn_up_left(Anchor id = Anchor::_) {
			WarGrey::SCADA::ITurtle::turn_up_left(_I(id));
			return this;

		}

		WarGrey::SCADA::Turtle<Anchor>* turn_right_up(Anchor id = Anchor::_) {
			WarGrey::SCADA::ITurtle::turn_right_up(_I(id));
			return this;
		}

		WarGrey::SCADA::Turtle<Anchor>* turn_up_right(Anchor id = Anchor::_) {
			WarGrey::SCADA::ITurtle::turn_up_right(_I(id));
			return this;
		}

	public:
		WarGrey::SCADA::Turtle<Anchor>* turn_down_left_up(Anchor id = Anchor::_) {
			WarGrey::SCADA::ITurtle::turn_down_left_up(_I(id));
			return this;
		}

		WarGrey::SCADA::Turtle<Anchor>* turn_down_right_up(Anchor id = Anchor::_) {
			WarGrey::SCADA::ITurtle::turn_down_right_up(_I(id));
			return this;
		}

		WarGrey::SCADA::Turtle<Anchor>* turn_up_left_down(Anchor id = Anchor::_) {
			WarGrey::SCADA::ITurtle::turn_up_left_down(_I(id));
			return this;
		}

		WarGrey::SCADA::Turtle<Anchor>* turn_up_right_down(Anchor id = Anchor::_) {
			WarGrey::SCADA::ITurtle::turn_up_right_down(_I(id));
			return this;
		}

		WarGrey::SCADA::Turtle<Anchor>* turn_left_down_right(Anchor id = Anchor::_) {
			WarGrey::SCADA::ITurtle::turn_left_down_right(_I(id));
			return this;
		}

		WarGrey::SCADA::Turtle<Anchor>* turn_left_up_right(Anchor id = Anchor::_) {
			WarGrey::SCADA::ITurtle::turn_left_up_right(_I(id));
			return this;
		}

		WarGrey::SCADA::Turtle<Anchor>* turn_right_down_left(Anchor id = Anchor::_) {
			WarGrey::SCADA::ITurtle::turn_right_down_left(_I(id));
			return this;
		}

		WarGrey::SCADA::Turtle<Anchor>* turn_right_up_left(Anchor id = Anchor::_) {
			WarGrey::SCADA::ITurtle::turn_right_up_left(_I(id));
			return this;
		}

	protected:
		virtual ~Turtle() noexcept {}
	};
}
