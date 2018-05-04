#pragma once

#include "universe.hxx"
#include "forward.hpp"
#include "sprite.hpp"
#include "box.hpp"

namespace WarGrey::SCADA {
    #define GRAPHLETS_LENGTH(a) (sizeof(a) / sizeof(IGraphlet*))

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
		WarGrey::SCADA::Syslog* get_logger() override;

	public:
		virtual void own_caret(bool is_own) {}

	public:
		IGraphletInfo * info;
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
		
		void set_value(T value, bool force_update = false) {
			if ((this->value != value) || force_update) {
				this->value = value;
				this->on_value_change(value);
			}
		}
		
	protected:
		virtual void on_value_change(T value) {}

	private:
		T value;
	};

	template<typename State, typename Style>
	private class IStatelet abstract : public virtual WarGrey::SCADA::IGraphlet {
	public:
		IStatelet(State default_state, Style (*make_default_style)(State)) {
			this->default_state = ((default_state == State::_) ? 0 : static_cast<unsigned int>(default_state));
			this->current_state = this->default_state;

			for (State s = static_cast<State>(0); s < State::_; s++) {
				this->set_style(s, make_default_style(s));
			}

			/** WARNING
			 * invoking `on_state_change` here has no effect
			 * since its virtual and here is inside the constructor
			 */
		}

	public:
		void set_state(State state) {
			unsigned int new_state = ((state == State::_) ? this->default_state : static_cast<unsigned int>(state));
			
			if (this->current_state != new_state) {
				this->current_state = new_state;
				this->on_state_change(static_cast<State>(new_state));
			}
		}

		State get_state() {
			return static_cast<State>(this->current_state);
		}

		void set_style(State state, Style& style) {
			this->styles[(state == State::_) ? this->current_state : static_cast<unsigned int>(state)] = style;
		}

		const Style& get_style(State state = State::_) {			
			return this->styles[(state == State::_) ? this->current_state : static_cast<unsigned int>(state)];
		}

	protected:
		virtual void on_state_change(State state) {}

	private:
		unsigned int default_state;
		unsigned int current_state;
		Style styles[static_cast<unsigned int>(State::_)];
	};

	/************************************************************************************************/
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
