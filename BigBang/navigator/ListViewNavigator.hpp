#pragma once

#include "navigator/IUniverseNavigator.hpp"

namespace WarGrey::SCADA {
	private class ListViewNavigator : public WarGrey::SCADA::IUniverseNavigator {
	public:
		ListViewNavigator(Windows::UI::Xaml::Controls::ListView^ master = nullptr);

	public:
		void insert(WarGrey::SCADA::IPlanet* planet) override;
		void select(WarGrey::SCADA::IPlanet* planet) override;
		int selected_index() override;

	public:
		Windows::UI::Xaml::UIElement^ display_element() override;

	private:
		Windows::UI::Xaml::Controls::ListView^ master;
		Platform::Object^ listener;
	};
}
