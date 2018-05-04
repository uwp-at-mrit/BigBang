#include <map>

#include "application.hxx"
#include "configuration.hpp"
#include "plc.hpp"

#include "planet.hpp"
#include "timer.hxx"
#include "brushes.hxx"

#include "frame/navigatorbar.hxx"
#include "frame/statusbar.hpp"
#include "page/homepage.hpp"
#include "page/airconditioner.hpp"

using namespace WarGrey::SCADA;

using namespace Windows::Foundation;

using namespace Windows::UI::Input;
using namespace Windows::UI::Xaml;
using namespace Windows::UI::Xaml::Input;
using namespace Windows::UI::Xaml::Controls;

using namespace Microsoft::Graphics::Canvas;
using namespace Microsoft::Graphics::Canvas::Brushes;

static PLCMaster* plc_master = nullptr;

template<class Bar>
private ref class BarUniverse sealed : public UniverseDisplay {
internal:
	BarUniverse(Bar* bar, Platform::String^ name) : UniverseDisplay(make_system_logger(default_logging_level, name)) {
		this->bar = bar;
		this->add_planet(this->bar);
	}

internal:
	Bar* get_universe() {
		return this->bar;
	}

private:
	Bar* bar;
};

private ref class PageUniverse sealed : public UniverseDisplay, public INavigatorAction {
public:
	PageUniverse(Platform::String^ name) : UniverseDisplay(make_system_logger(default_logging_level, name)) {}

public:
	virtual void on_navigate(Yacht page) {
		this->transfer_to(static_cast<int>(page));
	}

protected:
	void construct() override {
		for (Yacht page = Yacht::HomePage; page < Yacht::_; page++) {
			Platform::String^ name = page.ToString();

			switch (page) {
			case Yacht::AirConditioner: this->add_planet(new AirConditioner(plc_master, name)); break;
			default: this->add_planet(new Homepage(name)); break;
			}
		}
	}
};

private ref class Yacht63FT sealed : public StackPanel {
public:
	Yacht63FT() : StackPanel() {
		this->Margin = ThicknessHelper::FromUniformLength(0.0);
		this->Orientation = ::Orientation::Vertical;
		this->HorizontalAlignment = ::HorizontalAlignment::Center;
		this->VerticalAlignment = ::VerticalAlignment::Center;
	}

public:
	void initialize_component(Size region) {
		Platform::String^ name = "Yacht63FT";
		float fit_width = application_fit_size(screen_width);
		float fit_height = application_fit_size(screen_height);
		float fit_nav_height = application_fit_size(screen_navigator_height);
		float fit_bar_height = application_fit_size(screen_statusbar_height);

		plc_master = new PLCMaster(make_system_logger(default_logging_level, name + ":PLC"));

		this->timeline = ref new CompositeTimerAction();
		this->workspace = ref new PageUniverse(name);
		this->navigatorbar = ref new BarUniverse<Navigatorbar>(new Navigatorbar(this->workspace), name);
		this->statusbar = ref new BarUniverse<Statusbar>(new Statusbar(plc_master), name);

		this->load_display(this->navigatorbar, fit_width, fit_nav_height);
		this->load_display(this->workspace, fit_width, fit_height - fit_nav_height - fit_bar_height);
		this->load_display(this->statusbar, fit_width, fit_bar_height);
		this->timer = ref new Timer(this->timeline, frame_per_second);

		this->KeyDown += ref new KeyEventHandler(this->workspace, &UniverseDisplay::on_char);
	}

private:
	void load_display(UniverseDisplay^ display, float width, float height) {
		display->width = width;
		display->height = height;

		this->timeline->append_timer_action(display);
		this->Children->Append(display->canvas);
	}

private:
	WarGrey::SCADA::Timer^ timer;
	WarGrey::SCADA::CompositeTimerAction^ timeline;

private:
	BarUniverse<Navigatorbar>^ navigatorbar;
	PageUniverse^ workspace;
	BarUniverse<Statusbar>^ statusbar;
};

int main(Platform::Array<Platform::String^>^ args) {
	return launch_universal_windows_application<Yacht63FT, true>(remote_test_server);
}
