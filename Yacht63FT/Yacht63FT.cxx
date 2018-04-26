#include "application.hxx"
#include "configuration.hpp"
#include "plc.hpp"

#include "planet.hpp"
#include "timer.hxx"

#include "page/homepage.hpp"
#include "page/statusbar.hpp"

using namespace WarGrey::SCADA;

using namespace Windows::Foundation;

using namespace Windows::UI::Input;
using namespace Windows::UI::Xaml;
using namespace Windows::UI::Xaml::Media;
using namespace Windows::UI::Xaml::Input;
using namespace Windows::UI::Xaml::Controls;

using namespace Microsoft::Graphics::Canvas;

private ref class Universe sealed : public WarGrey::SCADA::UniverseDisplay {
public:
	virtual ~Universe() {
		if (this->device != nullptr) {
			delete this->device;
		}
	}

	Universe(Platform::String^ name) : UniverseDisplay(make_system_logger(default_logging_level, name)) {
		Syslog* alarm = make_system_logger(default_logging_level, name + ":PLC");

		this->timer = ref new Timer(this, 4);
		//this->device = new PLCMaster(alarm);
	}

protected:
	void construct() override {
		this->add_planet(new Homepage());
	}

private:
	WarGrey::SCADA::Timer^ timer;

private:
	PLCMaster* device;
};

private ref class StatusUniverse sealed : public WarGrey::SCADA::UniverseDisplay {
public:
	virtual ~StatusUniverse() {}

	StatusUniverse(Platform::String^ name) : UniverseDisplay(make_system_logger(default_logging_level, name)) {}

protected:
	void construct() override {
		this->add_planet(new Statusbar());
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
	void initialize_component(Windows::Foundation::Size region) {
		float fit_width = application_fit_size(screen_width);
		float fit_height = application_fit_size(screen_height);
		float fit_nav_height = application_fit_size(screen_navigator_height);
		float fit_bar_height = application_fit_size(screen_statusbar_height);
		
		this->navigator = ref new StackPanel();
		this->workspace = ref new Universe("Yacht63FT");
		this->statusbar = ref new StatusUniverse(this->workspace->get_logger()->get_name());

		this->navigator->Width = fit_width;
		this->navigator->Height = fit_nav_height;
		this->navigator->Orientation = ::Orientation::Horizontal;
		this->navigator->Margin = ThicknessHelper::FromUniformLength(0.0);
		this->navigator->Children->Append(this->workspace->navigator);

		this->workspace->width = fit_width;
		this->workspace->height = fit_height - fit_nav_height - fit_bar_height;
		this->statusbar->width = fit_width;
		this->statusbar->height = fit_bar_height;

		this->Children->Append(this->navigator);
		this->Children->Append(this->workspace->canvas);
		this->Children->Append(this->statusbar->canvas);

		this->KeyDown += ref new KeyEventHandler(this->workspace, &UniverseDisplay::on_char);
	}

private:
	WarGrey::SCADA::UniverseDisplay^ workspace;
	WarGrey::SCADA::UniverseDisplay^ statusbar;
	StackPanel^ navigator;
};

int main(Platform::Array<Platform::String^>^ args) {
	return launch_universal_windows_application<Yacht63FT, true>(remote_test_server);
}
