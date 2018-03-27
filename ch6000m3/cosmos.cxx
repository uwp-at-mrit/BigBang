#include "cosmos.hxx"

#include "timer.hxx"
#include "configuration.hpp"

#include "page/hp_single.hpp"
#include "page/graphlets.hpp"

using namespace WarGrey::SCADA;

using namespace Windows::Foundation;

using namespace Windows::UI::Input;
using namespace Windows::UI::Xaml;
using namespace Windows::UI::Xaml::Controls;
using namespace Windows::UI::Xaml::Input;

using namespace Microsoft::Graphics::Canvas;

/*************************************************************************************************/
private ref class Universe sealed : public WarGrey::SCADA::UniverseDisplay {
public:
	Universe(Platform::String^ name) : UniverseDisplay(make_system_logger(default_logging_level, name)) {
		this->timer = ref new Timer(this, 8);
	}

protected:
	void construct() override {
		this->add_planet(new HPSingle(remote_test_server));
		this->add_planet(new GraphletOverview());
	}

private:
	WarGrey::SCADA::Timer^ timer;
};

/*************************************************************************************************/
Cosmos::Cosmos() : SplitView() {
	this->Margin = ThicknessHelper::FromUniformLength(0.0);
	this->PanePlacement = SplitViewPanePlacement::Left;
	this->DisplayMode = SplitViewDisplayMode::Overlay;
	this->OpenPaneLength = 48;
	this->IsPaneOpen = false;

	this->PointerMoved += ref new PointerEventHandler(this, &Cosmos::on_pointer_moved);
}

void Cosmos::initialize_component(Size region) {
	this->universe = ref new Universe("CH6000M3");
	this->Content = this->universe->canvas;
	this->Pane = this->universe->navigator;

	// TODO: Why SplitView::Content cannot do it on its own?
	this->KeyDown += ref new KeyEventHandler(this->universe, &UniverseDisplay::on_char);
}

void Cosmos::on_pointer_moved(Platform::Object^ sender, PointerRoutedEventArgs^ args) {
	auto pt = args->GetCurrentPoint(this);
	float x = pt->Position.X;

	if (!pt->Properties->IsLeftButtonPressed) {
		if (x <= this->Margin.Left) {
			this->IsPaneOpen = true;
			args->Handled = true;
		} else if (x > this->OpenPaneLength) {
			this->IsPaneOpen = false;
			args->Handled = true;
		}
	}
}
