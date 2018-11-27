#pragma once

#include "planet.hpp"
#include "syslog.hpp"

namespace WarGrey::SCADA {
	private class ISatellite abstract : public WarGrey::SCADA::Planet {
	public:
		ISatellite(WarGrey::SCADA::Syslog* logger, Platform::String^ caption, unsigned int initial_mode = 0U);
		ISatellite(WarGrey::SCADA::Log level, Platform::String^ caption, unsigned int initial_mode = 0U);
		
	public:
		void notify_surface_ready() override;
		bool surface_ready() override;

	public:
		void hide();
		void show();

	public:
		virtual void fill_satellite_extent(float* width, float* height) = 0;
		virtual void fill_satellite_border(Windows::UI::Xaml::Thickness& border);
		virtual void fill_satellite_padding(Windows::UI::Xaml::Thickness& padding);

	public:
		virtual void on_satellite_showing() {}
		virtual void on_satellite_shown() {}
		virtual bool can_satellite_hiding() { return true; }
		virtual void on_satellite_hiden() {}

	protected:
		virtual void on_surface_ready() {}

	private:
		Windows::UI::Xaml::Controls::Flyout^ orbit;
		bool ready;
	};

	template<typename ID>
	private class ICreditSatellite abstract : public WarGrey::SCADA::ISatellite {
	public:
		ICreditSatellite(WarGrey::SCADA::Syslog* logger, Platform::String^ caption, unsigned int initial_mode = 0U)
			: ISatellite(logger, caption, initial_mode), pending(true) {}

		ICreditSatellite(WarGrey::SCADA::Log level, Platform::String^ caption, unsigned int initial_mode = 0U)
			: ISatellite(level, caption, initial_mode), pending(true) {}

	public:
		ID get_id() {
			return this->channel;
		}

		void switch_id(ID id) {
			if (id != this->channel) {
				this->channel = id;

				if (this->surface_ready()) {
					this->on_id_changed(this->channel);
				} else {
					this->pending = true;
				}
			}
		}

	protected:
		void on_surface_ready() override {
			if (this->pending) {
				this->pending = false;
				this->on_id_changed(this->channel);
			}
		}

	protected:
		virtual void on_id_changed(ID channel) = 0;

	protected:
		ID channel;

	private:
		bool pending;
	};
}
