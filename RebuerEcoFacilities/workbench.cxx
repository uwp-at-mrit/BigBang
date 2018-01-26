#include "workbench.hxx"
#include "tongue.hpp"
#include "B.hpp"

using namespace WarGrey::SCADA;

using namespace Windows::Foundation;

using namespace Windows::UI::Xaml;
using namespace Windows::UI::Xaml::Controls;

/*************************************************************************************************/
private ref class Universe sealed : public WarGrey::SCADA::UniverseDisplay {
public:
	Universe(Platform::String^ name) : UniverseDisplay(name, 4) {}

public:
	void construct() override {
		this->add_planet(new BWorkbench("B1", "192.168.0.188"));
		this->add_planet(new BWorkbench("B2", "192.168.0.114"));
		this->add_planet(new BWorkbench("B3", "192.168.1.128"));
		this->add_planet(new BWorkbench("B4", "192.168.8.114"));
	}
};

/*************************************************************************************************/
Workbench::Workbench() : SplitView() {
	this->Margin = ThicknessHelper::FromUniformLength(4.0);
	this->PanePlacement = SplitViewPanePlacement::Left;
	this->DisplayMode = SplitViewDisplayMode::Overlay;
	this->OpenPaneLength = 48;
	this->IsPaneOpen = false;
}

void Workbench::initialize_component(Size region) {
	this->universe = ref new Universe("Workbench");
	this->Content = this->universe->canvas;
	this->Pane = this->universe->navigator;
}

void Workbench::suspend(Windows::ApplicationModel::SuspendingOperation^ op) {
	// TODO: Save application state and stop any background activity.
	// Do not assume that the application will be terminated or resumed with the contents of memory still intact.
}
