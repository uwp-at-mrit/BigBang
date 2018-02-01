#pragma once

namespace WarGrey::SCADA {
	private ref class NumpadFlyout sealed : public Windows::UI::Xaml::Controls::Flyout {
	public:
		NumpadFlyout();

	private:
		Windows::UI::Xaml::Controls::GridView^ numboard;
	};
}
