#include "application.hxx"
#include "configuration.hpp"
#include "plc.hpp"

#include "planet.hpp"
#include "timer.hxx"

#include "page/homepage.hpp"


using namespace WarGrey::SCADA;

using namespace Windows::Foundation;

using namespace Windows::UI::Input;
using namespace Windows::UI::Xaml;
using namespace Windows::UI::Xaml::Controls;
using namespace Windows::UI::Xaml::Input;

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

		this->timer = ref new Timer(this, 2);
		this->device = new PLCMaster(alarm);
	}

protected:
	void construct() override {
		this->add_planet(new Homepage(this->device));
	}

private:
	WarGrey::SCADA::Timer^ timer;

private:
	PLCMaster* device;
};

private ref class StatusUniverse sealed : public WarGrey::SCADA::UniverseDisplay {
public:
	virtual ~StatusUniverse() {
	}

	StatusUniverse(Platform::String^ name) : UniverseDisplay(make_system_logger(default_logging_level, name)) {}

protected:
	void construct() override {}
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
		this->navigator = ref new StackPanel();
		this->workspace = ref new Universe("Yacht63FT");
		this->statusbar = ref new StatusUniverse("Yacht63FT");

		this->navigator->MinWidth = 1024;
		this->navigator->MinHeight = 90;
		this->navigator->Orientation = ::Orientation::Horizontal;
		this->navigator->Margin = ThicknessHelper::FromUniformLength(0.0);
		this->navigator->Children->Append(this->workspace->navigator);

		this->workspace->min_width = 1024;
		this->workspace->min_height = 400;
		this->statusbar->min_width = 1024;
		this->statusbar->min_height = 215;

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
