#include "console.hxx"
#include "tongue.hpp"
#include "syslog.hpp"
#include "time.hpp"
#include "B.hpp"

using namespace WarGrey::SCADA;

using namespace Windows::Foundation;
using namespace Windows::ApplicationModel;

using namespace Windows::UI::Xaml;
using namespace Windows::UI::Xaml::Controls;
using namespace Windows::UI::Xaml::Input;
using namespace Windows::UI::Xaml::Media::Media3D;

using namespace Windows::UI::Xaml::Media::Animation;

using namespace Microsoft::Graphics::Canvas;
using namespace Microsoft::Graphics::Canvas::UI;
using namespace Microsoft::Graphics::Canvas::UI::Xaml;
using namespace Microsoft::Graphics::Canvas::Brushes;
using namespace Microsoft::Graphics::Canvas::Geometry;

/*************************************************************************************************/
private ref class Universe sealed : public WarGrey::SCADA::UniverseDisplay {
public:
	Universe(SplitView^ parent) : UniverseDisplay(parent, 16, "Console") {}

public:
	void construct() override {
		this->add_planet(new BSegment(RR::B1.ToString(), "192.168.0.188"));
		this->add_planet(new BSegment(RR::B2.ToString(), "192.168.1.114"));
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

	this->flash = ref new Storyboard();
	this->transform = ref new CompositeTransform3D();

	this->ManipulationMode = ManipulationModes::TranslateX;
	this->ManipulationDelta += ref new ManipulationDeltaEventHandler(this, &Console::animate);
	this->ManipulationCompleted += ref new ManipulationCompletedEventHandler(this, &Console::animating);
}

void Console::initialize_component(Size region) {
	ListView^ navigator = ref new ListView();
	
	navigator->SelectionMode = ListViewSelectionMode::Single;

	for (size_t i = 0; i < static_cast<unsigned int>(RR::Count); i++) {
		this->labels[i] = ref new TextBlock();
		this->labels[i]->Text = speak(speak(static_cast<RR>(i).ToString()));
		navigator->Items->Append(this->labels[i]);
	}

	this->universe = ref new Universe(this);
}

void Console::switch_console(unsigned int idx) {
}

void Console::switch_console(RR id) {
	this->switch_console(static_cast<unsigned int>(id));
}

void Console::animate(Platform::Object^ sender, ManipulationDeltaRoutedEventArgs^ e) {
	this->transform->TranslateX += e->Delta.Translation.X;
}

void Console::animating(Platform::Object^ sender, ManipulationCompletedRoutedEventArgs^ e) {
	
}

void Console::animated(Platform::Object^ sender, Platform::Object^ e) {
	double delta = this->transform->TranslateX;

	if (delta < -128) {
		this->universe->enter_critical_section();
		this->universe->transfer_next();
		this->universe->leave_critical_section();
	} else if (delta > 128) {
		this->universe->enter_critical_section();
		this->universe->transfer_previous();
		this->universe->leave_critical_section();
	}

	this->transform->TranslateX = 0;
	this->Content->Transform3D = this->transform;
}

void Console::suspend(Windows::ApplicationModel::SuspendingOperation^ op) {
	// TODO: Save application state and stop any background activity.
	// Do not assume that the application will be terminated or resumed with the contents of memory still intact.
}
