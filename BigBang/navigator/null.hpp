#pragma once

#include "navigator/navigator.hpp"

namespace WarGrey::SCADA {
	private class NullNavigator : public WarGrey::SCADA::IUniverseNavigator {
	public:
		void insert(WarGrey::SCADA::IPlanet* planet) override;
		void select(WarGrey::SCADA::IPlanet* planet) override;
		int selected_index() override;

	public:
		Windows::UI::Xaml::Controls::Control^ user_interface() override;
	};
}
