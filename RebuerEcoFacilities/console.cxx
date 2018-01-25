#include "console.hxx"
#include "tongue.hpp"
#include "time.hpp"
#include "B.hpp"

using namespace WarGrey::SCADA;

using namespace Windows::Foundation;

using namespace Windows::UI::Xaml;
using namespace Windows::UI::Xaml::Controls;
using namespace Windows::UI::Xaml::Input;
using namespace Windows::UI::Xaml::Media::Animation;

using namespace Microsoft::Graphics::Canvas::UI::Xaml;

/*************************************************************************************************/
private ref class Universe sealed : public WarGrey::SCADA::UniverseDisplay {
public:
	Universe(Platform::String^ name) : UniverseDisplay(4, name) {}

public:
	void construct() override {
		this->add_planet(new BSegment(RR::B1.ToString(), "192.168.0.188"));
		this->add_planet(new BSegment(RR::B2.ToString(), "192.168.0.114"));
		this->add_planet(new BSegment(RR::B3.ToString(), "192.168.1.128"));
		this->add_planet(new BSegment(RR::B4.ToString(), "192.168.8.114"));
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
	this->ManipulationDelta += ref new ManipulationDeltaEventHandler(this, &Console::animating);
	this->ManipulationCompleted += ref new ManipulationCompletedEventHandler(this, &Console::animated);
}

void Console::initialize_component(Size region) {
	ListView^ navigator = ref new ListView();
	
	navigator->SelectionMode = ListViewSelectionMode::Single;

	for (size_t i = 0; i < static_cast<unsigned int>(RR::Count); i++) {
		this->labels[i] = ref new TextBlock();
		this->labels[i]->Text = speak(speak(static_cast<RR>(i).ToString()));
		navigator->Items->Append(this->labels[i]);
	}

	this->universe = ref new Universe("Console");
	this->Content = this->universe->canvas;
	this->Pane = navigator;
}

void Console::animating(Platform::Object^ sender, ManipulationDeltaRoutedEventArgs^ e) {
}

void Console::animated(Platform::Object^ sender, ManipulationCompletedRoutedEventArgs^ e) {
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
