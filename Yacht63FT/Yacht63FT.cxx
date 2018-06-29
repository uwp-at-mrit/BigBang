#include "application.hxx"
#include "configuration.hpp"
#include "plc.hpp"

#include "planet.hpp"
#include "timer.hxx"
#include "brushes.hxx"

#include "frame/navigatorbar.hxx"
#include "frame/statusbar.hpp"
#include "page/homepage.hpp"
#include "page/generator.hpp"
#include "page/propeller.hpp"
#include "page/propulsion.hpp"
#include "page/airconditioner.hpp"
#include "page/log.hpp"
#include "page/operation.hpp"
#include "page/gauge.hpp"

using namespace WarGrey::SCADA;

using namespace Windows::Foundation;

using namespace Windows::UI::Input;
using namespace Windows::UI::Xaml;
using namespace Windows::UI::Xaml::Input;
using namespace Windows::UI::Xaml::Controls;

using namespace Microsoft::Graphics::Canvas;
using namespace Microsoft::Graphics::Canvas::Brushes;

static PLCMaster* plc_master = nullptr;

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
			case Yacht::Generator: this->add_planet(new GeneratorPage(plc_master, name)); break;
			case Yacht::Propeller: this->add_planet(new PropellerPage(plc_master, name)); break;
			case Yacht::Propulsion: this->add_planet(new PropulsionPage(plc_master, name)); break;
			case Yacht::AirConditioner: this->add_planet(new ACPage(plc_master, name)); break;
			case Yacht::Operation: this->add_planet(new OperationPage(plc_master, name)); break;
			case Yacht::Event: this->add_planet(new EventPage(plc_master, name)); break;
			case Yacht::Gauge: this->add_planet(new GaugePage(plc_master, name)); break;
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
		Syslog* default_logger = make_system_logger(default_logging_level, name);
		float fit_width = screen_to_application_size(screen_width);
		float fit_height = screen_to_application_size(screen_height);
		float fit_nav_height = design_to_application_height(screen_navigator_height);
		float fit_bar_height = design_to_application_height(screen_statusbar_height);

		plc_master = new PLCMaster(make_system_logger(default_logging_level, name + ":PLC"));

		this->timeline = ref new CompositeTimerAction();
		this->workspace = ref new PageUniverse(name);
		this->navigatorbar = ref new UniverseDisplay(default_logger, new Navigatorbar(plc_master, this->workspace));
		this->statusbar = ref new UniverseDisplay(default_logger, new Statusbar(plc_master));

		this->load_display(this->navigatorbar, fit_width, fit_nav_height);
		this->load_display(this->workspace, fit_width, fit_height - fit_nav_height - fit_bar_height);
		this->load_display(this->statusbar, fit_width, fit_bar_height);
		this->timer = ref new Timer(this->timeline, frame_per_second);

		this->KeyDown += ref new KeyEventHandler(this->workspace, &UniverseDisplay::on_char);
		this->workspace->navigator->SelectionChanged += ref new SelectionChangedEventHandler(this, &Yacht63FT::do_notify);
	}

private:
	void load_display(UniverseDisplay^ display, float width, float height) {
		display->width = width;
		display->height = height;

		this->timeline->append_timer_action(display);
		this->Children->Append(display->canvas);
	}

	void do_notify(Platform::Object^ sender, SelectionChangedEventArgs^ args) {
		if (this->navigatorbar != nullptr) {
			Yacht page = static_cast<Yacht>(this->workspace->current_planet_index);
			Navigatorbar* bar = static_cast<Navigatorbar*>(this->navigatorbar->current_planet);
			
			bar->on_navigated_to(page);
		}
	}

private:
	WarGrey::SCADA::Timer^ timer;
	WarGrey::SCADA::CompositeTimerAction^ timeline;

private:
	UniverseDisplay^ navigatorbar;
	PageUniverse^ workspace;
	UniverseDisplay^ statusbar;
};

/*************************************************************************************************/
int main(Platform::Array<Platform::String^>^ args) {
	return launch_universal_windows_application<Yacht63FT, true>(default_logging_level, remote_test_server);
}
