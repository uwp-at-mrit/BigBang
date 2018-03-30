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
		// `id` is designed for user-applications, in order to distinguish instances of a graphlet class.
		// User-Applications should define and maintain the enumerations on their own.
		long int id = -1L;

	public:
		WarGrey::SCADA::Syslog* get_logger() override;

	public:
		virtual void own_caret(bool is_own) {}

    public:
        IGraphletInfo* info;
    };

	private class IPipelet : public WarGrey::SCADA::IGraphlet {
	public:
		virtual Windows::Foundation::Rect get_input_port() = 0;
		virtual Windows::Foundation::Rect get_output_port() = 0;
	};

	template<typename T>
	private class IScalelet : public WarGrey::SCADA::IGraphlet {
	public:
		T get_scale() {
			return this->scale;
		}
		
		void set_scale(T scale, bool force_update = false) {
			if ((this->scale != scale) || force_update) {
				this->scale = scale;
				this->update_scale();
			}
		}
		
	protected:
		virtual void update_scale() = 0;

	protected:
		T scale;
	};

	template<typename State, typename Style>
	private class IStatelet : public WarGrey::SCADA::IGraphlet {
	public:
		template<typename Lambda>
		IStatelet(State default_state, Lambda make_default_style) {
			this->default_state = ((default_state == State::_) ? 0 : static_cast<unsigned long long>(default_state));
			this->current_state = this->default_state;

			for (State s = static_cast<State>(0); s < State::_; s++) {
				this->set_style(s, make_default_style(s));
			}

			/** WARNING
			 *    invoking `on_state_change` here has no effect
			 *    since its virtual and here is inside the constructor
			 */
		}

	public:
		void set_state(State state) {
			unsigned long long new_state = ((state == State::_) ? this->default_state : static_cast<unsigned long long>(state));
			
			if (this->current_state != new_state) {
				this->current_state = new_state;
				this->on_state_change(static_cast<State>(new_state));
			}
		}

		State get_state() {
			return static_cast<State>(this->current_state);
		}

		void set_style(State state, Style& style) {
			this->styles[(state == State::_) ? this->current_state : static_cast<unsigned long long>(state)] = style;
		}

		const Style& get_style(State state = State::_) {			
			return this->styles[(state == State::_) ? this->current_state : static_cast<unsigned long long>(state)];
		}

	protected:
		virtual void on_state_change(State state) {}

	private:
		unsigned long long default_state;
		unsigned long long current_state;
		Style styles[static_cast<unsigned long long>(State::_)];
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
