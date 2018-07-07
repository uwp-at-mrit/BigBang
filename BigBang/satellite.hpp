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
		void show();
		void hide();

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

	template<class Master, typename ID>
	private class ICreditSatellite abstract : public ISatellite {
	public:
		ICreditSatellite(WarGrey::SCADA::Syslog* logger, Master* master, Platform::String^ caption, unsigned int initial_mode = 0U)
			: ISatellite(logger, caption, initial_mode), pending(true), master(master) {}

		ICreditSatellite(WarGrey::SCADA::Log level, Master* master, Platform::String^ caption, unsigned int initial_mode = 0U)
			: ISatellite(level, caption, initial_mode), pending(true), master(master) {}

	public:
		ID get_channel() {
			return this->channel;
		}

		void switch_channel(ID id) {
			if (id != this->channel) {
				this->channel = id;

				if (this->surface_ready()) {
					this->on_channel_changed(this->channel);
				} else {
					this->pending = true;
				}
			}
		}

	protected:
		void on_surface_ready() override {
			if (this->pending) {
				this->pending = false;
				this->on_channel_changed(this->channel);
			}
		}

	protected:
		virtual void on_channel_changed(ID channel) = 0;

	protected:
		Master* master;
		ID channel;

	private:
		bool pending;
	};
}
