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

	template<typename Status, typename Style>
	private class IStatuslet abstract : public virtual WarGrey::SCADA::IGraphlet {
	public:
		IStatuslet(Status default_status, Style (*make_default_style)(Status)) {
			this->default_status = ((default_status == Status::_) ? 0 : static_cast<unsigned int>(default_status));
			this->current_status = this->default_status;

			for (Status s = static_cast<Status>(0); s < Status::_; s++) {
				this->set_style(s, make_default_style(s));
			}

			/** WARNING
			 * invoking `on_status_change` here has no effect
			 * since its virtual and here is inside the constructor
			 */
		}

	public:
		void set_status(Status status) {
			unsigned int new_status = ((status == Status::_) ? this->default_status : static_cast<unsigned int>(status));
			
			if (this->current_status != new_status) {
				this->current_status = new_status;
				this->on_status_change(static_cast<Status>(new_status));
			}
		}

		Status get_status() {
			return static_cast<Status>(this->current_status);
		}

		void set_style(Status status, Style& style) {
			this->styles[(status == Status::_) ? this->current_status : static_cast<unsigned int>(status)] = style;
		}

		const Style& get_style(Status status = Status::_) {			
			return this->styles[(status == Status::_) ? this->current_status : static_cast<unsigned int>(status)];
		}

	protected:
		virtual void on_status_change(Status status) {}

	private:
		unsigned int default_status;
		unsigned int current_status;
		Style styles[static_cast<unsigned int>(Status::_)];
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
