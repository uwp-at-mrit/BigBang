#include "console.hxx"
#include "tongue.hpp"
#include "B.hpp"

using namespace WarGrey::SCADA;

using namespace Windows::Foundation;
using namespace Windows::ApplicationModel;

using namespace Windows::UI::Xaml;
using namespace Windows::UI::Xaml::Controls;
using namespace Windows::UI::Xaml::Input;
using namespace Windows::UI::Xaml::Media::Media3D;

using namespace Microsoft::Graphics::Canvas;
using namespace Microsoft::Graphics::Canvas::UI;
using namespace Microsoft::Graphics::Canvas::UI::Xaml;
using namespace Microsoft::Graphics::Canvas::Brushes;
using namespace Microsoft::Graphics::Canvas::Geometry;

/*************************************************************************************************/
Console::Console() : SplitView() {
	this->Margin = ThicknessHelper::FromUniformLength(4.0);
	this->PanePlacement = SplitViewPanePlacement::Left;

	this->DisplayMode = SplitViewDisplayMode::Overlay;
	this->OpenPaneLength = 48;
	this->IsPaneOpen = false;

	this->transform = ref new CompositeTransform3D();
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

	// this->universes[0] = new BSegment(this->voids[0], RR::A.ToString(), "192.168.0.188");
	// this->universes[0] = new BSegment(this->voids[0], RR::B1.ToString(), "192.168.1.114");
	// this->universes[1] = new BSegment(this->voids[0], RR::B2.ToString(), "192.168.1.188");
	// this->universes[2] = new BSegment(this->voids[0], RR::B3.ToString(), "192.168.1.114");
	// this->universes[3] = new BSegment(this->voids[0], RR::B4.ToString(), "192.168.1.114");

	// this->Pane = navigator;
	// this->switch_console(0);

	this->universe = ref new UniverseDisplay(this, new BSegment(RR::B1.ToString(), "192.168.1.114"), 16);
}

void Console::switch_console(unsigned int idx) {
}

void Console::switch_console(RR id) {
	this->switch_console(static_cast<unsigned int>(id));
}

void Console::animating(Platform::Object^ sender, Windows::UI::Xaml::Input::ManipulationDeltaRoutedEventArgs^ e) {
	this->transform->TranslateX += e->Delta.Translation.X;
	this->Content->Transform3D = this->transform;
}

void Console::animated(Platform::Object^ sender, Windows::UI::Xaml::Input::ManipulationCompletedRoutedEventArgs^ e) {
	this->Content->Transform3D = nullptr;
}

void Console::suspend(Windows::ApplicationModel::SuspendingOperation^ op) {
	// TODO: Save application state and stop any background activity.
	// Do not assume that the application will be terminated or resumed with the contents of memory still intact.
}
