#include "application.hxx"
#include "configuration.hpp"
#include "plc.hpp"

#include "planet.hpp"
#include "timer.hxx"

#include "page/hydraulics.hpp"
#include "page/hopper_doors.hpp"
#include "page/sealed_waters.hpp"
#include "page/draughts.hpp"
#include "page/lubricatings.hpp"
#include "page/charges.hpp"
#include "page/discharges.hpp"
#include "page/flushs.hpp"
#include "page/dredges.hpp"

#include "splash.hpp"
#include "gallery.hpp"

using namespace WarGrey::SCADA;

using namespace Windows::Foundation;

using namespace Windows::UI::Input;
using namespace Windows::UI::Xaml;
using namespace Windows::UI::Xaml::Input;
using namespace Windows::UI::Xaml::Controls;

using namespace Microsoft::Graphics::Canvas;

private ref class Universe sealed : public UniverseDisplay {
public:
	virtual ~Universe() {
		if (this->device != nullptr) {
			delete this->device;
		}
	}

	Universe(Platform::String^ name) : UniverseDisplay(make_system_logger(default_logging_level, name)) {
		Syslog* logger = make_system_logger(default_logging_level, name + ":PLC");

		this->timer = ref new Timer(this, frame_per_second);
		this->device = new PLCMaster(logger, PLCMasterMode::Release);
	}

protected:
	void construct() override {
		//this->add_planet(new SplashScreen(620.0F));
		//this->add_planet(new SplashScreen(1240.0F, 0.0F));
		
		this->add_planet(new HydraulicsPage(this->device));
		this->add_planet(new ChargesPage(this->device));
		this->add_planet(new DredgesPage(this->device));
		this->add_planet(new DischargesPage(this->device));
		this->add_planet(new SealedWaterPage(this->device));
		this->add_planet(new FlushsPage(this->device));
		this->add_planet(new DredgesPage(this->device, DragView::Left));
		this->add_planet(new HopperDoorsPage(this->device));
		this->add_planet(new LubricatingsPage(this->device));
		this->add_planet(new DraughtsPage(this->device));
		this->add_planet(new DredgesPage(this->device, DragView::Right));

		this->add_planet(new Gallery());

		this->transfer_to(2);
	}

private:
	Timer^ timer;

private:
	PLCMaster* device;
};

private ref class CH6000m3 sealed : public SplitView {
public:
	CH6000m3() : SplitView() {
		this->Margin = ThicknessHelper::FromUniformLength(0.0);
		this->PanePlacement = SplitViewPanePlacement::Left;
		this->DisplayMode = SplitViewDisplayMode::Overlay;
		this->IsPaneOpen = false;

		// for Mouse pointer
		this->PointerMoved += ref new PointerEventHandler(this, &CH6000m3::on_pointer_moved);

		// for TouchScreen pointer, but others can also be satisfied
		this->AddHandler(UIElement::PointerPressedEvent, ref new PointerEventHandler(this, &CH6000m3::on_pointer_pressed), true);
	}

public:
	void construct(Platform::String^ name, Size region) {
		this->universe = ref new Universe(name);
		this->Content = this->universe->canvas;
		this->Pane = this->universe->navigator;

		// TODO: Why SplitView::Content cannot do it on its own?
		this->KeyDown += ref new KeyEventHandler(this->universe, &UniverseDisplay::on_char);
	}

private:
	void on_pointer_pressed(Platform::Object^ sender, PointerRoutedEventArgs^ args) {
		auto pt = args->GetCurrentPoint(this);
		float x = pt->Position.X;

		if (pt->Properties->IsLeftButtonPressed) {
			if (x <= tiny_font_size) {
				this->IsPaneOpen = true;
				args->Handled = true;
			} else if (x > this->OpenPaneLength) {
				this->IsPaneOpen = false;
				args->Handled = true;
			}
		}
	}
	
	void on_pointer_moved(Platform::Object^ sender, PointerRoutedEventArgs^ args) {
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

private:
	UniverseDisplay^ universe;
};

int main(Platform::Array<Platform::String^>^ args) {
	return launch_universal_windows_application<CH6000m3>(default_logging_level, remote_test_server);
}
