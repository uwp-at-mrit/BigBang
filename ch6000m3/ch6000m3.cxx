#include "application.hxx"
#include "configuration.hpp"
#include "iotables/macro_keys.hpp"
#include "plc.hpp"

#include "planet.hpp"
#include "timer.hxx"
#include "dirotation.hpp"

#include "page/hydraulics.hpp"
#include "page/hopper_doors.hpp"
#include "page/glands.hpp"
#include "page/draughts.hpp"
#include "page/lubricatings.hpp"
#include "page/charges.hpp"
#include "page/discharges.hpp"
#include "page/flushs.hpp"
#include "page/dredges.hpp"

#include "splash.hpp"
#include "gallery.hpp"
#include "test/performance.hpp"

using namespace WarGrey::SCADA;

using namespace Windows::System;
using namespace Windows::Foundation;

using namespace Windows::UI::Core;
using namespace Windows::UI::Input;
using namespace Windows::UI::Xaml;
using namespace Windows::UI::Xaml::Input;
using namespace Windows::UI::Xaml::Controls;

using namespace Microsoft::Graphics::Canvas;

private ref class DredgerUniverse : public UniverseDisplay {
public:
	virtual ~DredgerUniverse() {
		if (this->device != nullptr) {
			delete this->device;
		}
	}

internal:
	DredgerUniverse(Platform::String^ name) : UniverseDisplay(make_system_logger(default_logging_level, name)) {
		Platform::String^ localhost = system_ipv4_address();
		Syslog* logger = make_system_logger(default_logging_level, name + ":PLC");
		PLCMasterMode mode = PLCMasterMode::User;

		this->timer = ref new Timer(this, frame_per_second);
		this->device = new PLCMaster(logger);

		for (unsigned int idx = 0; idx < sizeof(root_machines) / sizeof(Platform::String^); idx++) {
			if (localhost->Equals(root_machines[idx])) {
				mode = PLCMasterMode::Root;
				break;
			}
		}

		this->device->set_mode(mode);
	}

public:
	void switch_plc_master_mode() {
		if (this->device != nullptr) {
			switch (this->device->get_mode()) {
			case PLCMasterMode::Debug: this->device->set_mode(PLCMasterMode::Root); break;
			case PLCMasterMode::Root: this->device->set_mode(PLCMasterMode::User); break;
			case PLCMasterMode::User: this->device->set_mode(PLCMasterMode::Debug); break;
			}
		}
	}

protected:
	void construct() override {
		//this->add_planet(new SplashScreen(620.0F));
		//this->add_planet(new SplashScreen(1240.0F, 0.0F));
		
		this->add_planet(new HydraulicsPage(this->device)); // 0
		this->add_planet(new ChargesPage(this->device)); // 1
		this->add_planet(new DredgesPage(this->device)); // 2
		this->add_planet(new DischargesPage(this->device)); // 3
		this->add_planet(new GlandsPage(this->device)); // 4
		this->add_planet(new FlushsPage(this->device)); // 5
		this->add_planet(new DredgesPage(this->device, DragView::Left)); // 6
		this->add_planet(new HopperDoorsPage(this->device)); // 7
		this->add_planet(new LubricatingsPage(this->device)); // 8
		this->add_planet(new DraughtsPage(this->device)); // 9
		this->add_planet(new DredgesPage(this->device, DragView::Right)); // 10

		this->add_planet(new Gallery());

		if (system_ipv4_address()->Equals("192.168.0.10")) {
			this->add_planet(new PerformancePage(this->device));
		}

		this->transfer_to(6);
	}

protected private:
	PLCMaster* device;

private:
	Timer^ timer;
};

/*************************************************************************************************/
private class PageEventListener : public PLCConfirmation {
public:
	PageEventListener(unsigned int idx) : dbidx(idx), page(2 /* the dredging page */) {}

public:
	void on_analog_input(const uint8* DB2, size_t count2, const uint8* DB203, size_t count203, WarGrey::SCADA::Syslog* logger) override {
		this->page = int(DBD(DB2, this->dbidx));
	}

public:
	int get_target_page() {
		return page;
	}

private:
	unsigned int dbidx;
	int page;
};

private ref class DashboardUniverse sealed : public DredgerUniverse {
public:
	virtual ~DashboardUniverse() {
		if (this->page_turner != nullptr) {
			delete this->page_turner;
		}
	}

	DashboardUniverse(Platform::String^ name, unsigned int dbidx) : DredgerUniverse(name) {
		this->page_turner = new PageEventListener(dbidx);
		this->device->append_confirmation_receiver(this->page_turner);
	}

public:
	void update(long long count, long long interval, long long uptime) override {
		this->transfer_to(this->page_turner->get_target_page());
	}

private:
	PageEventListener* page_turner;
};

/*************************************************************************************************/
private ref class CH6000m3 sealed : public SplitView {
public:
	CH6000m3() : SplitView() {
		this->Margin = ThicknessHelper::FromUniformLength(0.0);
		this->PanePlacement = SplitViewPanePlacement::Left;
		this->DisplayMode = SplitViewDisplayMode::Overlay;
		this->IsPaneOpen = false;

		this->PointerMoved += ref new PointerEventHandler(this, &CH6000m3::on_pointer_moved);
		this->PointerReleased += ref new PointerEventHandler(this, &CH6000m3::on_pointer_released);

		//this->AddHandler(UIElement::PointerPressedEvent, ref new PointerEventHandler(this, &CH6000m3::on_pointer_pressed), true);
		//this->AddHandler(UIElement::PointerReleasedEvent, ref new PointerEventHandler(this, &CH6000m3::on_pointer_released), true);
	}

public:
	void construct(Platform::String^ name, Size region) {
		Platform::String^ localhost = system_ipv4_address();

		if (localhost->Equals("192.168.0.11")) {
			this->universe = ref new DashboardUniverse(name, left_paging_key);
		} else if (localhost->Equals("192.168.0.12")) {
			this->universe = ref new DashboardUniverse(name, right_paging_key);
		} else {
			this->universe = ref new DredgerUniverse(name);
		}

		this->Content = this->universe->canvas;
		this->Pane = this->universe->navigator;

		// TODO: Why SplitView::Content cannot do it on its own?
		this->universe->register_virtual_keydown_event_handler(this);
	}

private:
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

	void on_pointer_released(Platform::Object^ sender, PointerRoutedEventArgs^ args) {
		if ((args->KeyModifiers & VirtualKeyModifiers::Shift) == VirtualKeyModifiers::Shift) {
			this->universe->switch_plc_master_mode();
		} else {
			auto pt = args->GetCurrentPoint(this);
			float x = pt->Position.X;

			if (pt->Properties->PointerUpdateKind == PointerUpdateKind::LeftButtonReleased) {
				if (x <= normal_font_size) {
					this->IsPaneOpen = true;
					args->Handled = true;
				} else if (x > this->OpenPaneLength) {
					this->IsPaneOpen = false;
					args->Handled = true;
				}
			}
		}
	}

private:
	DredgerUniverse^ universe;
};

int main(Platform::Array<Platform::String^>^ args) {
	return launch_universal_windows_application<CH6000m3>(default_logging_level, remote_test_server);
}
