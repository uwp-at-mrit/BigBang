#include <algorithm>
#include <list>

#include "system.hpp"
#include "time.hpp"

using namespace WarGrey::SCADA;

using namespace Windows::Foundation;
using namespace Windows::Graphics::Display;

using namespace Windows::Devices::Power;
using namespace Windows::Devices::WiFi;

using namespace Windows::Networking;
using namespace Windows::Networking::Connectivity;

using namespace Windows::UI;
using namespace Windows::UI::Xaml;
using namespace Windows::UI::Xaml::Controls;
using namespace Windows::UI::ViewManagement;

typedef TypedEventHandler<Battery^, Platform::Object^> BatteryUpdateHandler;
typedef TypedEventHandler<WiFiAdapter^, Platform::Object^> WiFiUpdateHandler;

static UISettings^ sysUI = nullptr;

static inline Size adjust_size(float Width, float Height, FrameworkElement^ workspace) {
    auto margin = workspace->Margin;

    float width = Width - float(margin.Left + margin.Right);
    float height = Height - float(margin.Top + margin.Bottom);
    return Size(width, height);
}

/*************************************************************************************************/
Size WarGrey::SCADA::adjusted_workspace_size(Rect region, FrameworkElement^ workspace) {
	return adjust_size(region.Width, region.Height, workspace);
}

Size WarGrey::SCADA::system_screen_size() {
    auto master = DisplayInformation::GetForCurrentView();
    auto scaling = float(master->RawPixelsPerViewPixel);

    return { float(master->ScreenWidthInRawPixels) / scaling, float(master->ScreenHeightInRawPixels) / scaling };
}

Color WarGrey::SCADA::system_color(UIColorType type) {
    if (sysUI == nullptr) {
        sysUI = ref new UISettings();
    }

    return sysUI->GetColorValue(type);
}

Color WarGrey::SCADA::system_color(UIElementType type) {
    if (sysUI == nullptr) {
        sysUI = ref new UISettings();
    }

    return sysUI->UIElementColor(type);
}

/*************************************************************************************************/
long long WarGrey::SCADA::system_available_storage_size() {
	return 0LL;
}

float WarGrey::SCADA::system_battery_capacity(float defval_if_no_battery) {
	auto info = Battery::AggregateBattery->GetReport();
	auto maybe_remaining = info->RemainingCapacityInMilliwattHours;
	float capacity = defval_if_no_battery;

	if (maybe_remaining != nullptr) {
		auto remaining = float(info->RemainingCapacityInMilliwattHours->Value);
		auto full = float(info->FullChargeCapacityInMilliwattHours->Value);

		capacity = remaining / full;
	}

	return capacity;
}

char WarGrey::SCADA::system_wifi_signal_strength(char defval_if_no_wifi) {
	auto nics = NetworkInformation::GetConnectionProfiles();
	char signal = defval_if_no_wifi;

	for (unsigned int i = 0; i < nics->Size; ++i) {
		auto nic = nics->GetAt(i);
		if (nic->IsWlanConnectionProfile) {
			signal = nic->GetSignalBars()->Value;
			break;
		}
	}

	return signal;
}

Platform::String^ WarGrey::SCADA::system_ipv4_address(Platform::String^ defval_if_no_nic) {
	auto names = NetworkInformation::GetHostNames();
	Platform::String^ ipv4 = defval_if_no_nic;

	for (unsigned int i = 0; i < names->Size; ++i) {
		auto host = names->GetAt(i);
		if (host->Type == HostNameType::Ipv4) {
			ipv4 = host->RawName;
			break;
		}
	}

	return ipv4;
}

/*************************************************************************************************/
private ref class SystemStatus sealed {
public:
	static SystemStatus^ get_status_info_provider() {
		static SystemStatus^ singleton = ref new SystemStatus();

		return singleton;
	}

internal:
	void add_status_listener(ISystemStatusListener* listener) {
		if (listener != nullptr) {
			this->listeners.push_back(listener);

			listener->on_timestamp_changed(update_nowstamp(false));
			listener->on_battery_capacity_changed(system_battery_capacity());
			listener->on_wifi_signal_strength_changed(system_wifi_signal_strength());
			listener->on_available_storage_changed(system_available_storage_size());
			listener->on_ipv4_address_changed(system_ipv4_address());
		}
	}

private:
	void report_timestamp(Platform::Object^ sender, Platform::Object^ e) {
		int l00ns;
		Platform::String^ timestamp = update_nowstamp(false, &l00ns);

		this->clock->Interval = TimeSpan{ 10000000LL - l00ns };

		for (auto listener : this->listeners) {
			listener->on_timestamp_changed(timestamp);
		}
	}

	void report_powerinfo(Battery^ sender, Platform::Object^ e) {
		float capacity = system_battery_capacity();

		for (auto listener : this->listeners) {
			listener->on_battery_capacity_changed(capacity);
		}
	}

	void report_wifiinfo(WiFiAdapter^ sender, Platform::Object^ e) {
		char signal = system_wifi_signal_strength();

		for (auto listener : this->listeners) {
			listener->on_wifi_signal_strength_changed(signal);
		}
	}

private:
	SystemStatus() {
		this->clock = ref new DispatcherTimer();

		Battery::AggregateBattery->ReportUpdated += ref new BatteryUpdateHandler(this, &SystemStatus::report_powerinfo);
		// WiFiAdapter::AvailableNetworksChanged += ref new WiFiUpdateHandler(this, &SystemStatus::report_wifiinfo);

		this->clock->Tick += ref new EventHandler<Object^>(this, &SystemStatus::report_timestamp);
		this->report_timestamp(nullptr, nullptr);
		this->clock->Start();
	}

	~SystemStatus() {
		this->clock->Stop();
	}


private:
	std::list<ISystemStatusListener*> listeners;
	DispatcherTimer^ clock;
};

void WarGrey::SCADA::register_system_status_listener(ISystemStatusListener* listener) {
	SystemStatus::get_status_info_provider()->add_status_listener(listener);
}
