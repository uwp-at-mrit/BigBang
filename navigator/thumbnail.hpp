#pragma once

#include "navigator/navigator.hpp"
#include "syslog.hpp"

namespace WarGrey::SCADA {
	ref class NavigationDisplay;

	private class ThumbnailNavigator : public WarGrey::SCADA::IUniverseNavigator {
	public:
		ThumbnailNavigator(WarGrey::SCADA::Log level, Platform::String^ title,
			float ratio, float cell_width = 128.0F, unsigned int column = 2U, float gapsize = 16.0F);

	public:
		void insert(WarGrey::SCADA::IPlanet* planet) override;
		void select(WarGrey::SCADA::IPlanet* planet) override;
		int selected_index() override;

	public:
		Windows::UI::Xaml::Controls::Control^ user_interface() override;

	private:
		WarGrey::SCADA::NavigationDisplay^ master;
	};
}
