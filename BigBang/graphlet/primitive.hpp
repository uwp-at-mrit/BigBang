#pragma once

#include "universe.hxx"
#include "forward.hpp"
#include "sprite.hpp"
#include "box.hpp"
#include "slot.hpp"

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
		void notify_ready();
		void notify_updated();

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
		
		void set_value(T value0, bool force_update = false) {
			T value = this->adjusted_value(value0);

			if ((this->value != value) || force_update) {
				this->value = value;
				this->on_value_changed(value);
				this->notify_updated();
			}
		}

		void set_value(T value0, WarGrey::SCADA::GraphletAnchor anchor, bool force_update = false) {
			if (this->info == nullptr) {
				this->set_value(value0, force_update);
			} else {
				T value = this->adjusted_value(value0);

				if ((this->value != value) || force_update) {
					float anchor_x, anchor_y;

					this->info->master->fill_graphlet_location(this, &anchor_x, &anchor_y, anchor);
					this->value = value;
					this->on_value_changed(value);

					this->info->master->begin_update_sequence();
					this->notify_updated();
					this->info->master->move_to(this, anchor_x, anchor_y, anchor);
					this->info->master->end_update_sequence();
				}
			}
		}
		
	protected:
		virtual void on_value_changed(T value) {}

	protected:
		virtual T adjusted_value(T value) { return value; }

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
		float get_percentage() {
			float flmin = static_cast<float>(this->vmin);
			float flmax = static_cast<float>(this->vmax);
			float v = static_cast<float>(this->get_value());

			return ((this->vmin == this->vmax) ? 1.0F : ((v - flmin) / (flmax - flmin)));
		}

	protected:
		T adjusted_value(T v) override {
			if (v > this->vmax) {
				v = this->vmax;
			} else if (v < this->vmin) {
				v = this->vmin;
			}

			return v;
		}

	private:
		T vmin;
		T vmax;
	};

	template<typename Status, typename Style>
	private class IStatuslet abstract : public virtual WarGrey::SCADA::IGraphlet {
	public:
		IStatuslet() : WarGrey::SCADA::IStatuslet<Status, Style>(Status::_) {}

		IStatuslet(Status status0) {
			this->default_status = ((status0 == Status::_) ? 0 : static_cast<unsigned int>(status0));
			this->current_status = this->default_status;

			/** WARNING
			 * invoking `apply_style` and `on_status_changed` here has no effect
			 * since they are virtual and here is inside the constructor
			 *
			 * `update_status` is designed for children to achieve the goal.
			 */
		}

	public:
		void set_status(Status status) {
			unsigned int new_status = ((status == Status::_) ? this->default_status : static_cast<unsigned int>(status));

			if (this->current_status != new_status) {
				this->current_status = new_status;
				this->update_status();
				this->notify_updated();
			}
		}

		Status get_status() {
			return static_cast<Status>(this->current_status);
		}

		void set_style(Status status, Style& style) {
			unsigned int idx = (status == Status::_) ? this->current_status : static_cast<unsigned int>(status);

			this->styles[idx] = style;
			this->style_ready[idx] = false;

			if (idx == this->current_status) {
				this->notify_updated();
			}
		}

		Style& get_style(Status status = Status::_) {
			unsigned int idx = (status == Status::_) ? this->current_status : static_cast<unsigned int>(status);

			if (!this->style_ready[idx]) {
				this->prepare_style(static_cast<Status>(idx), this->styles[idx]);
				this->style_ready[idx] = true;
			}

			return this->styles[idx];
		}

	protected:
		void update_status() {
			this->apply_style(this->get_style());
			this->on_status_changed(static_cast<Status>(this->current_status));
		}

	protected:
		virtual void prepare_style(Status status, Style& style) = 0;
		virtual void on_status_changed(Status status) {}
		virtual void apply_style(Style& style) {}

	private:
		unsigned int default_status;
		unsigned int current_status;
		Style styles[static_cast<unsigned int>(Status::_)];
		bool style_ready[static_cast<unsigned int>(Status::_)];
	};

	template<typename Status, typename Style>
	private class ISymbollet abstract : public WarGrey::SCADA::IStatuslet<Status, Style> {
	public:
		ISymbollet(float radius, double degrees) : size(radius * 2.0F), degrees(degrees) {}

		ISymbollet(Status default_status, float radius, double degrees)
			: IStatuslet<Status, Style>(default_status), size(radius * 2.0F), degrees(degrees) {}

	public:
		void ISymbollet::fill_extent(float x, float y, float* w = nullptr, float* h = nullptr) override {
			SET_BOXES(w, h, this->size);
		}

	public:
		double get_direction_degrees() { return this->degrees; }

	protected:
		double degrees;
		float size;
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
