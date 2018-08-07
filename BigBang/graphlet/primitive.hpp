#pragma once

#include "universe.hxx"
#include "forward.hpp"
#include "sprite.hpp"
#include "enum.hpp"
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
		WarGrey::SCADA::IPlanet* master();
		WarGrey::SCADA::Syslog* get_logger() override;

	public:
		virtual void own_caret(bool is_own) {}

	public:
		void notify_ready();
		void notify_updated();
		void moor(WarGrey::SCADA::GraphletAnchor anchor);

	public:
		float available_visible_width(float here_x = 0.0F);
		float available_visible_height(float here_y = 0.0F);
		float sketch_to_application_width(float sketch_width);
		float sketch_to_application_height(float sketch_height);

	public:
		IGraphletInfo * info;

	private:
		float anchor_x;
		float anchor_y;
		WarGrey::SCADA::GraphletAnchor anchor;
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
			T value = this->adjusted_value(value0);

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
			float flmin = _F(this->vmin);
			float flmax = _F(this->vmax);
			float v = _F(this->get_value());

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

	protected:
		T vmin;
		T vmax;
	};

	template<typename Status, typename Style>
	private class IStatuslet abstract : public virtual WarGrey::SCADA::IGraphlet {
	public:
		IStatuslet() : WarGrey::SCADA::IStatuslet<Status, Style>(Status::_) {}

		IStatuslet(Status status0) {
			this->default_status = ((status0 == Status::_) ? 0 : _I(status0));
			this->current_status = this->default_status;
		}

	public:
		void sprite() override {
			this->update_status();
		}

	public:
		void set_status(Status status) {
			unsigned int new_status = ((status == Status::_) ? this->default_status : _I(status));

			if (this->current_status != new_status) {
				this->current_status = new_status;
				this->update_status();
				this->notify_updated();
			}
		}

		Status get_status() {
			return _E(Status, this->current_status);
		}

		void set_style(Status status, Style& style) {
			unsigned int idx = (status == Status::_) ? this->current_status : _I(status);

			this->styles[idx] = style;
			this->style_ready[idx] = false;

			if (idx == this->current_status) {
				this->notify_updated();
			}
		}

		Style& get_style(Status status = Status::_) {
			unsigned int idx = (status == Status::_) ? this->current_status : _I(status);

			if (!this->style_ready[idx]) {
				this->prepare_style(_E(Status, idx), this->styles[idx]);
				this->style_ready[idx] = true;
			}

			return this->styles[idx];
		}

	protected:
		void update_status() {
			this->apply_style(this->get_style());
			this->on_status_changed(_E(Status, this->current_status));
		}

	protected:
		virtual void prepare_style(Status status, Style& style) = 0;
		virtual void on_status_changed(Status status) {}
		virtual void apply_style(Style& style) {}

	private:
		unsigned int default_status;
		unsigned int current_status;
		Style styles[_N(Status)];
		bool style_ready[_N(Status)];
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
