#include "workbench.hxx"
#include "tongue.hpp"

using namespace WarGrey::SCADA;

using namespace Windows::Foundation;

using namespace Windows::UI::Input;
using namespace Windows::UI::Xaml;
using namespace Windows::UI::Xaml::Controls;
using namespace Windows::UI::Xaml::Input;

/*************************************************************************************************/
private ref class Universe sealed : public WarGrey::SCADA::UniverseDisplay {
public:
	Universe(Platform::String^ name) : UniverseDisplay(name, 16) {}

public:
	void construct() override {
	}
};

/*************************************************************************************************/
Workbench::Workbench() : SplitView() {
	this->Margin = ThicknessHelper::FromUniformLength(0.0);
	this->PanePlacement = SplitViewPanePlacement::Left;
	this->DisplayMode = SplitViewDisplayMode::Overlay;
	this->OpenPaneLength = 48;
	this->IsPaneOpen = false;

	this->PointerMoved += ref new PointerEventHandler(this, &Workbench::on_pointer_moved);
}

void Workbench::initialize_component(Size region) {
	this->universe = ref new Universe("Workbench");
	this->Content = this->universe->canvas;
	this->Pane = this->universe->navigator;

	// TODO: Why SplitView::Content cannot do it on its own?
	this->KeyDown += ref new KeyEventHandler(this->universe, &UniverseDisplay::on_char);
}

void Workbench::on_pointer_moved(Platform::Object^ sender, PointerRoutedEventArgs^ args) {
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
