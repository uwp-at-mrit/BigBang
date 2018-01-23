#include "console.hxx"
#include "tongue.hpp"
#include "time.hpp"
#include "B.hpp"

using namespace WarGrey::SCADA;

using namespace Windows::Foundation;
using namespace Windows::ApplicationModel;

using namespace Windows::UI::Xaml;
using namespace Windows::UI::Xaml::Controls;
using namespace Windows::UI::Xaml::Input;
using namespace Windows::UI::Xaml::Media;
using namespace Windows::UI::Xaml::Media::Animation;

using namespace Microsoft::Graphics::Canvas;
using namespace Microsoft::Graphics::Canvas::UI;
using namespace Microsoft::Graphics::Canvas::UI::Xaml;
using namespace Microsoft::Graphics::Canvas::Brushes;
using namespace Microsoft::Graphics::Canvas::Geometry;

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

	this->duration = make_timespan_from_ms(256);
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

	this->universe = ref new Universe("Console");
	this->Content = this->universe->canvas;
}

void Console::animate(Platform::Object^ sender, ManipulationDeltaRoutedEventArgs^ e) {
	this->transformX += e->Delta.Translation.X;
}

void Console::animating(Platform::Object^ sender, ManipulationCompletedRoutedEventArgs^ e) {
	float width = this->universe->actual_width;

	this->nt_action = ref new DoubleAnimation();
	this->nt_action->Duration = duration;
	this->nt_action->From = 0.0;
	this->nt_action->AutoReverse = true;
	this->nt_action->To = -double(width);

	this->nt_story = ref new Storyboard();
	this->nt_story->Children->Append(this->nt_action);
	this->nt_story->SetTarget(this->nt_action, this->Content);
	this->nt_story->SetTargetProperty(this->nt_action, "(UIElement.RenderTransform).(TranslateTransform.X)");
	this->nt_story->Completed += ref new EventHandler<Platform::Object^>(this, &Console::animated);

	this->nt_story->Begin();
}

void Console::animated(Platform::Object^ sender, Platform::Object^ e) {
	float delta = this->transformX;

	if (delta < -128.0F) {
		this->universe->enter_critical_section();
		this->universe->transfer_next();
		this->universe->leave_critical_section();
	} else if (delta > 128.0F) {
		this->universe->enter_critical_section();
		this->universe->transfer_previous();
		this->universe->leave_critical_section();
	}

	this->transformX = 0.0F;
}

void Console::suspend(Windows::ApplicationModel::SuspendingOperation^ op) {
	// TODO: Save application state and stop any background activity.
	// Do not assume that the application will be terminated or resumed with the contents of memory still intact.
}
