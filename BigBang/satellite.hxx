#pragma once

#include "planet.hpp"
#include "syslog.hpp"

namespace WarGrey::SCADA {
	private class ISatellite abstract : public WarGrey::SCADA::Planet {
	public:
		ISatellite(Platform::String^ caption, unsigned int initial_mode = 0U)
			: Planet(caption, initial_mode), ready(false) {}

	public:
		void notify_surface_ready() override;
		bool surface_ready() override;
		bool shown();

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
		void hide();

	protected:
		virtual void on_surface_ready() {}

	private:
		bool ready;
	};

	private ref class SatelliteOrbit sealed : public Windows::UI::Xaml::Controls::Flyout {
	public:
		virtual ~SatelliteOrbit();

	internal:
		SatelliteOrbit(WarGrey::SCADA::ISatellite* entity, WarGrey::SCADA::Syslog* logger);
		SatelliteOrbit(WarGrey::SCADA::ISatellite* entity, WarGrey::SCADA::Log level, Platform::String^ topic = nullptr);

	internal:
		WarGrey::SCADA::ISatellite* get_satellite();
		WarGrey::SCADA::Syslog* get_logger();

	internal:
		void show(WarGrey::SCADA::IPlanet* master);

	public:
		bool shown();

	private:
		void construct(WarGrey::SCADA::ISatellite* entity, WarGrey::SCADA::Syslog* logger);

	private:
		void on_opening(Platform::Object^ target, Platform::Object^ args);
		void on_opened(Platform::Object^ target, Platform::Object^ args);
		void on_closing(Windows::UI::Xaml::Controls::Primitives::FlyoutBase^ target, Windows::UI::Xaml::Controls::Primitives::FlyoutBaseClosingEventArgs^ args);
		void on_closed(Platform::Object^ target, Platform::Object^ args);

	private:
		WarGrey::SCADA::UniverseDisplay^ display;
		bool showing;
	};

	template<class Master, typename ID>
	private class ICreditSatellite abstract : public ISatellite {
	public:
		ICreditSatellite(Master* master, Platform::String^ caption, unsigned int initial_mode = 0U)
			: ISatellite(caption, initial_mode), pending(true), master(master) {}

	public:
		ID get_channel() {
			return this->channel;
		}

		void switch_channel(ID id) {
			if (id != channel) {
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
