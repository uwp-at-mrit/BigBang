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
#include "page/alogbook.hpp"
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

private ref class YachtDisplay : public UniverseDisplay {
internal:
	YachtDisplay(Platform::String^ name, IPlanet* first_planet = nullptr)
		: UniverseDisplay(DisplayFit::Fill, screen_width, screen_height, sketch_width, sketch_height,
			make_system_logger(default_logging_level, name), first_planet) {
	}
};

private ref class PageUniverse sealed : public YachtDisplay, public INavigatorAction {
public:
	PageUniverse(Platform::String^ name) : YachtDisplay(name) {}

public:
	virtual void on_navigate(Yacht page) {
		this->transfer_to(_I(page));
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
			case Yacht::Logbook: this->add_planet(new LogbookPage(plc_master, name)); break;
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
		float sketch_workspace_height = sketch_height - sketch_navigator_height - sketch_statusbar_height;
		
		plc_master = new PLCMaster(make_system_logger(default_logging_level, name + ":PLC"));

		this->timeline = ref new CompositeTimerAction();
		this->workspace = ref new PageUniverse(name + "[Workspace]");
		this->navigatorbar = ref new YachtDisplay(name + "[Navigator]", new Navigatorbar(plc_master, this->workspace));
		this->statusbar = ref new YachtDisplay(name + "[Statusbar]", new Statusbar(plc_master));

		this->load_display(this->navigatorbar, screen_width, sketch_navigator_height, 0.0F);
		this->load_display(this->workspace, screen_width, sketch_workspace_height, -21.0F);
		this->load_display(this->statusbar, screen_width, sketch_statusbar_height, 0.0F);
		this->timer = ref new Timer(this->timeline, frame_per_second);

		this->KeyDown += ref new KeyEventHandler(this->workspace, &UniverseDisplay::on_char);
		this->workspace->navigator->SelectionChanged += ref new SelectionChangedEventHandler(this, &Yacht63FT::do_notify);
	}

private:
	void load_display(UniverseDisplay^ display, float width, float height, float revise_height) {
		display->apply_source_size(width, height, 0.0F, revise_height);

		/** TODO
		* Why the workspace canvas resizes twice?
		* The second resizing enlarges the ActualHeight by about 20 pixels.
		*
		* `this->UseLayoutRounding = false` does not work.
		*/

		this->timeline->append_timer_action(display);
		this->Children->Append(display->canvas);
	}

	void do_notify(Platform::Object^ sender, SelectionChangedEventArgs^ args) {
		if (this->navigatorbar != nullptr) {
			Yacht page = _E(Yacht, this->workspace->current_planet_index);
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
	return launch_universal_windows_application<Yacht63FT, true>(default_logging_level, remote_test_server, "zh-CN");
}
