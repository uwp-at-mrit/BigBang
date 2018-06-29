#pragma once

#include "planet.hpp"
#include "syslog.hpp"

namespace WarGrey::SCADA {
	private class ISatellite abstract : public WarGrey::SCADA::Planet {
	public:
		ISatellite(Platform::String^ caption, unsigned int initial_mode = 0U)
			: Planet(caption, initial_mode) {}

	public:
		virtual void on_satellite_showing() {}
		virtual void on_satellite_shown() {}
		virtual bool on_satellite_hiding() { return true; }
		virtual void on_satellite_hiden() {}
	};

	private ref class SatelliteDisplay sealed : public Windows::UI::Xaml::Controls::Flyout {
	internal:
		SatelliteDisplay(float width, float height, WarGrey::SCADA::ISatellite* entity, WarGrey::SCADA::Syslog* logger);
		SatelliteDisplay(float width, float height, WarGrey::SCADA::ISatellite* entity, WarGrey::SCADA::Log level, Platform::String^ topic = nullptr);

	internal:
		WarGrey::SCADA::ISatellite* get_satellite();
		WarGrey::SCADA::Syslog* get_logger();

	internal:
		void show(WarGrey::SCADA::IPlanet* master);

	private:
		void construct(float width, float height, WarGrey::SCADA::ISatellite* entity, WarGrey::SCADA::Syslog* logger);

	private:
		void on_opening(Platform::Object^ target, Platform::Object^ args);
		void on_opened(Platform::Object^ target, Platform::Object^ args);
		void on_closing(Windows::UI::Xaml::Controls::Primitives::FlyoutBase^ target, Windows::UI::Xaml::Controls::Primitives::FlyoutBaseClosingEventArgs^ args);
		void on_closed(Platform::Object^ target, Platform::Object^ args);

	private:
		WarGrey::SCADA::UniverseDisplay^ display;
	};
}
