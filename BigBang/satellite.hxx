#pragma once

#include "planet.hpp"
#include "syslog.hpp"

namespace WarGrey::SCADA {
	private class ISatellite abstract : public WarGrey::SCADA::Planet {
	public:
		ISatellite(Platform::String^ caption, unsigned int initial_mode = 0U)
			: Planet(caption, initial_mode) {}

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

	private:
		void construct(WarGrey::SCADA::ISatellite* entity, WarGrey::SCADA::Syslog* logger);

	private:
		void on_opening(Platform::Object^ target, Platform::Object^ args);
		void on_opened(Platform::Object^ target, Platform::Object^ args);
		void on_closing(Windows::UI::Xaml::Controls::Primitives::FlyoutBase^ target, Windows::UI::Xaml::Controls::Primitives::FlyoutBaseClosingEventArgs^ args);
		void on_closed(Platform::Object^ target, Platform::Object^ args);

	private:
		WarGrey::SCADA::UniverseDisplay^ display;
	};

	template<typename ID>
	private class CreditSatellite : public ISatellite {
	public:
		CreditSatellite(Platform::String^ caption, unsigned int initial_mode = 0U)
			: ISatellite(caption, initial_mode), pending(true) {}

	public:
		void notify_ready_to_draw() override {
			if (this->pending) {
				this->pending = false;
				this->on_channel_changed(this->channel);
			}
		}

	public:
		ID get_channel() {
			return this->channel;
		}

		void switch_channel(ID id) {
			if (id != channel) {
				this->channel = id;

				if (this->ready()) {
					this->on_channel_changed(this->channel);
				} else {
					this->pending = true;
				}
			}
		}

	protected:
		virtual void on_channel_changed(ID id) {}

	private:
		ID channel;
		bool pending;
	};
}
