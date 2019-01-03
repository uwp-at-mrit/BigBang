#pragma once

#include "planet.hpp"
#include "syslog.hpp"

namespace WarGrey::SCADA {
	private class ITimeMachine abstract : public WarGrey::SCADA::Planet {
	public:
		ITimeMachine(WarGrey::SCADA::Syslog* logger, Platform::String^ caption, unsigned int initial_mode = 0U);
		ITimeMachine(WarGrey::SCADA::Log level, Platform::String^ caption, unsigned int initial_mode = 0U);
		
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
}
