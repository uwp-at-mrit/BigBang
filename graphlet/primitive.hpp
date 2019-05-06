#pragma once

#include "forward.hpp"
#include "sprite.hpp"

#include "datum/enum.hpp"
#include "datum/box.hpp"
#include "datum/slot.hpp"

namespace WarGrey::SCADA {
	private class IGraphletInfo abstract {
    public:
		virtual ~IGraphletInfo() noexcept {};
		IGraphletInfo(IPlanet* master) : master(master) {};
		
    public:
		IPlanet* master;
    };

	private class IGraphlet abstract : public WarGrey::SCADA::ISprite {
	public:
		virtual ~IGraphlet() noexcept;

	public:
		WarGrey::SCADA::IPlanet* master();
		WarGrey::SCADA::Syslog* get_logger() override;

	public:
		virtual void own_caret(bool is_own) {}

	public:
		void moor(WarGrey::SCADA::GraphletAnchor anchor);
		bool has_caret();

	public:
		float available_visible_width(float here_x = 0.0F);
		float available_visible_height(float here_y = 0.0F);
		float sketch_to_application_width(float sketch_width);
		float sketch_to_application_height(float sketch_height);

	public:
		void notify_ready();
		void notify_updated();

	public:
		IGraphletInfo * info;

	private:
		WarGrey::SCADA::GraphletAnchor anchor;
		float anchor_x;
		float anchor_y;
	};

	private class IPipelet abstract : public virtual WarGrey::SCADA::IGraphlet {
	public:
		virtual Windows::Foundation::Rect get_input_port() = 0;
		virtual Windows::Foundation::Rect get_output_port() = 0;
	};

	template<typename T>
	private class IValuelet abstract : public virtual WarGrey::SCADA::IGraphlet {
	public:
		T get_value() {
			return this->value;
		}
		
		void set_value(T value0, bool force_update = false) {
			this->set_value(value0, WarGrey::SCADA::GraphletAnchor::LT, force_update);
		}

		void set_value(T value0, WarGrey::SCADA::GraphletAnchor anchor, bool force_update = false) {
			T value = this->guarded_value(value0);

			if ((this->value != value) || force_update) {
				this->moor(anchor);
				this->value = value;
				this->on_value_changed(value);
				this->notify_updated();
			}
		}
		
	protected:
		virtual void on_value_changed(T value) {}

	protected:
		virtual T guarded_value(T value) { return value; }

	private:
		T value;
	};

	template<typename T>
	private class IRangelet abstract : public virtual WarGrey::SCADA::IValuelet<T> {
	public:
		IRangelet(T vmin, T vmax) {
			if (vmin <= vmax) {
				this->vmin = vmin;
				this->vmax = vmax;
			} else {
				this->vmin = vmax;
				this->vmax = vmin;
			}
		}

	public:
		void set_range(T vmin, T vmax, bool force_update = false) {
			this->set_range(vmin, vmax, WarGrey::SCADA::GraphletAnchor::LT, force_update);
		}

		void set_range(T vmin, T vmax, WarGrey::SCADA::GraphletAnchor anchor, bool force_update = false) {
			if (this->can_change_range()) {
				bool changed = false;

				if (vmin <= vmax) {
					if ((this->vmin != vmin) || (this->vmax != vmax)) {
						this->vmin = vmin;
						this->vmax = vmax;
						changed = true;
					}
				} else {
					if ((this->vmin != vmax) || (this->vmax != vmin)) {
						this->vmin = vmax;
						this->vmax = vmin;
						changed = true;
					}
				}

				if (force_update || changed) {
					this->moor(anchor);
					this->on_range_changed(this->vmin, this->vmax);
					this->notify_updated();
				}
			}
		}

		virtual void on_range_changed(T vmin, T vmax) {}

	public:
		double get_percentage(T value) {
			double flmin = double(this->vmin);
			double flrange = double(this->vmax) - flmin;
			double v = double(value);

			return (this->vmin == this->vmax) ? 1.0 : ((v - flmin) / flrange);
		}

		double get_percentage() {
			return this->get_percentage(this->get_value());
		}

	protected:
		virtual bool can_change_range() { return false; }

		T guarded_value(T v) override {
			if (v > this->vmax) {
				v = this->vmax;
			} else if (v < this->vmin) {
				v = this->vmin;
			}

			return v;
		}

	protected:
		T vmin;
		T vmax;
	};

	template<typename State, typename Style>
	private class IStatelet abstract : public virtual WarGrey::SCADA::IGraphlet {
	public:
		IStatelet() : IStatelet(State::_) {}

		IStatelet(State state0) {
			this->default_state = ((state0 == State::_) ? 0 : _I(state0));
			this->current_state = this->default_state;
		}

	public:
		void sprite_construct() override {
			this->update_state();
		}

	public:		
		void set_state(State state) {
			unsigned int new_state = ((state == State::_) ? this->default_state : _I(state));

			if (this->current_state != new_state) {
				this->current_state = new_state;
				this->update_state();
				this->notify_updated();
			}
		}

		void set_state(bool condition, State state) {
			if (condition) {
				this->set_state(state);
			}
		}

		void set_state(bool condition, State state_yes, State state_no) {
			if (condition) {
				this->set_state(state_yes);
			} else {
				this->set_state(state_no);
			}
		}

		State get_state() {
			return _E(State, this->current_state);
		}

		void set_style(State state, Style& style) {
			unsigned int idx = (state == State::_) ? this->current_state : _I(state);

			this->styles[idx] = style;
			this->style_ready[idx] = false;

			if (idx == this->current_state) {
				this->update_state();
				this->notify_updated();
			}
		}

		void set_style(Style& style) {
			for (State s = _E(State, 0); s < State::_; s++) {
				this->set_style(s, style);
			}
		}

		Style& get_style(State state = State::_) {
			unsigned int idx = (state == State::_) ? this->current_state : _I(state);

			if (!this->style_ready[idx]) {
				this->prepare_style(_E(State, idx), this->styles[idx]);
				this->style_ready[idx] = true;
			}

			return this->styles[idx];
		}

	protected:
		void update_state() {
			this->apply_style(this->get_style());
			this->on_state_changed(_E(State, this->current_state));
		}

	protected:
		virtual void prepare_style(State status, Style& style) = 0;
		virtual void on_state_changed(State status) {}
		virtual void apply_style(Style& style) {}

	private:
		unsigned int default_state;
		unsigned int current_state;
		Style styles[_N(State)];
		bool style_ready[_N(State)];
	};

	template<typename State, typename Style>
	private class ISymbollet abstract : public WarGrey::SCADA::IStatelet<State, Style> {
	public:
		ISymbollet(float radius, double degrees)
			: ISymbollet<State, Style>(State::_, radius, degrees) {}

		ISymbollet(State default_state, float radius, double degrees)
			: IStatelet<State, Style>(default_state)
			, radiusX(radius), radiusY(radius)
			, width(radius * 2.0F), height(radius * 2.0F), degrees(degrees) {}

		ISymbollet(float radiusX, float radiusY, double degrees, float proportion = 2.0F)
			: ISymbollet<State, Style>(State::_, radiusX, radiusY, degrees, proportion) {}

		ISymbollet(State default_state, float radiusX, float radiusY, double degrees, float proportion = 2.0F)
			: IStatelet<State, Style>(default_state)
			, radiusX(radiusX), radiusY(radiusY), degrees(degrees) {
			if (this->radiusY == 0.0F) {
				this->radiusY = this->radiusX * proportion;
			}

			{ // detect enclosing box
				Windows::Foundation::Rect box = WarGrey::SCADA::symbol_enclosing_box(
					this->radiusX, this->radiusY, this->degrees);

				this->width = box.Width;
				this->height = box.Height;
			}
		}

	public:
		void ISymbollet::fill_extent(float x, float y, float* w = nullptr, float* h = nullptr) override {
			SET_BOX(w, this->width);
			SET_BOX(h, this->height);
		}

	public:
		double get_direction_degrees() { return this->degrees; }
		float get_radiusX() { return this->radiusX; }
		float get_radiusY() { return this->radiusY; }

	protected:
		double degrees;
		float radiusX;
		float radiusY;
		float width;
		float height;
	};

	/************************************************************************************************/
	Windows::Foundation::Rect symbol_enclosing_box(float radiusX, float radiusY, double degrees);

	Windows::Foundation::Rect graphlet_enclosing_box(
		WarGrey::SCADA::IGraphlet* g, float x, float y,
		Windows::Foundation::Numerics::float3x2 tf);

	void pipe_connecting_position(
		WarGrey::SCADA::IPipelet* prev, WarGrey::SCADA::IPipelet* pipe,
		float* x, float* y, double factor_x = 0.5, double factor_y = 0.5);

	Windows::Foundation::Numerics::float2 pipe_connecting_position(
		WarGrey::SCADA::IPipelet* prev, WarGrey::SCADA::IPipelet* pipe,
		float x = 0.0F, float y = 0.0F, double factor_x = 0.5, double factor_y = 0.5);
}
