#include "console.hxx"
#include "tongue.hpp"
#include "B.hpp"

using namespace WarGrey::SCADA;

using namespace Windows::Foundation;

using namespace Windows::UI::Xaml;
using namespace Windows::UI::Xaml::Input;
using namespace Windows::UI::Xaml::Controls;

/*************************************************************************************************/
private ref class Universe sealed : public WarGrey::SCADA::UniverseDisplay {
public:
	Universe(Platform::String^ name) : UniverseDisplay(name, 4) {}

public:
	void construct() override {
		this->add_planet(new BSegment("B1", "192.168.0.188"));
		this->add_planet(new BSegment("B2", "192.168.1.114"));
		this->add_planet(new BSegment("B3", "192.168.1.128"));
		this->add_planet(new BSegment("B4", "192.168.8.114"));
	}
};

/*************************************************************************************************/
Console::Console() : SplitView() {
	this->Margin = ThicknessHelper::FromUniformLength(4.0);
	this->PanePlacement = SplitViewPanePlacement::Left;
	this->DisplayMode = SplitViewDisplayMode::Overlay;
	this->OpenPaneLength = 48;
	this->IsPaneOpen = false;

	this->ManipulationMode = ManipulationModes::TranslateX;
	this->ManipulationCompleted += ref new ManipulationCompletedEventHandler(this, &Console::transfer);
}

void Console::initialize_component(Size region) {
	this->universe = ref new Universe("Console");
	this->Content = this->universe->canvas;
	this->Pane = this->universe->navigator;
}

void Console::transfer(Platform::Object^ sender, ManipulationCompletedRoutedEventArgs^ e) {
	float width = this->universe->actual_width;
	float delta = e->Cumulative.Translation.X;
	float distance = width * 0.0618F;

	if (delta < -distance) {
		this->universe->transfer_next(256);
	} else if (delta > distance) {
		this->universe->transfer_previous(256);
	}

	e->Handled = true;
}

void Console::suspend(Windows::ApplicationModel::SuspendingOperation^ op) {
	// TODO: Save application state and stop any background activity.
	// Do not assume that the application will be terminated or resumed with the contents of memory still intact.
}
