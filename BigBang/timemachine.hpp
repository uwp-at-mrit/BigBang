#pragma once

#include "universe.hxx"
#include "syslog.hpp"

namespace WarGrey::SCADA {
	private class ITimeMachine abstract {
	public:
		ITimeMachine(WarGrey::SCADA::Syslog* logger = nullptr);

	public:
		virtual void construct() = 0;

	public:
		virtual void fill_timemachine_extent(float* width, float* height) = 0;
		virtual void fill_timemachine_border(Windows::UI::Xaml::Thickness& border);
		virtual void fill_timemachine_padding(Windows::UI::Xaml::Thickness& padding);

	public:
		virtual void on_timemachine_showing() {}
		virtual void on_timemachine_shown() {}
		virtual bool can_timemachine_hiding() { return true; }
		virtual void on_timemachine_hiden() {}

	public:
		void push_planet(WarGrey::SCADA::IPlanet* planet);
		WarGrey::SCADA::Syslog* get_logger();

	public:
		void notify_surface_ready();
		bool surface_ready();

	public:
		void hide();
		void show();

	protected:
		virtual void on_surface_ready() {}

	private:
		Windows::UI::Xaml::Controls::Flyout^ machine;
		WarGrey::SCADA::UniverseDisplay^ universe;
		bool ready;
	};
}
