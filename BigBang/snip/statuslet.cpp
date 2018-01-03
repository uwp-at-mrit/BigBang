#include <algorithm>
#include <ppltasks.h>

#include "text.hpp"
#include "paint.hpp"
#include "time.hpp"
#include "tongue.hpp"
#include "universe.hpp"
#include "snip/statuslet.hpp"

using namespace WarGrey::SCADA;

using namespace Concurrency;

using namespace Windows::UI;
using namespace Windows::UI::Xaml;
using namespace Windows::Foundation;

using namespace Windows::Devices::Power;
using namespace Windows::Devices::WiFi;

using namespace Windows::Networking;
using namespace Windows::Networking::Connectivity;

using namespace Microsoft::Graphics::Canvas;
using namespace Microsoft::Graphics::Canvas::Text;
using namespace Microsoft::Graphics::Canvas::Brushes;

typedef TypedEventHandler<Battery^, Platform::Object^> BatteryUpdateHandler;
typedef TypedEventHandler<WiFiAdapter^, Platform::Object^> WiFiUpdateHandler;

// delegate only accepts C++/CX class
ref class Status sealed {
public:
    TimeSpan update_timestamp() {
        int l00ns;

        this->timestamp = update_nowstamp(false, &l00ns);

        return TimeSpan{ 10000000 - l00ns };
    }

    void update_powerinfo() {
        auto info = Battery::AggregateBattery->GetReport();
		auto maybe_remaining = info->RemainingCapacityInMilliwattHours;

		if (maybe_remaining == nullptr) {
			this->powercapacity = speak("powerlabel") + "100%";
		} else {
			auto remaining = float(info->RemainingCapacityInMilliwattHours->Value);
			auto full = float(info->FullChargeCapacityInMilliwattHours->Value);

			this->powercapacity = speak("powerlabel") + round((remaining / full) * 100.0F).ToString() + "%";
		}
    }

    void update_wifiinfo(WiFiAdapter^ info) {
        auto nics = NetworkInformation::GetConnectionProfiles();
        Platform::String^ signal = speak("nowifi");

        for (unsigned int i = 0; i < nics->Size; ++i) {
            auto nic = nics->GetAt(i);
            if (nic->IsWlanConnectionProfile) {
                auto signal_bar = nic->GetSignalBars()->Value;
                signal = round(signal_bar / 5 * 100).ToString() + L"%";
                break;
            }
        }

        this->wifi_strength = speak("wifilabel") + signal;
    }

    void update_sdinfo() {
        this->storage = speak("sdlabel") + L"0MB";
    }

    void update_ipinfo() {
        auto names = NetworkInformation::GetHostNames();
        Platform::String^ ipv4 = speak("noipv4");

        for (unsigned int i = 0; i < names->Size; ++i) {
            auto host = names->GetAt(i);
            if (host->Type == HostNameType::Ipv4) {
                ipv4 = host->RawName;
                break;
            }
        }

        this->ipv4 = speak("ipv4label") + ipv4;
    }

internal:
    Status(Statusbarlet* master) {
        this->master = master;

        Battery::AggregateBattery->ReportUpdated += ref new BatteryUpdateHandler(this, &Status::refresh_powerinfo);
        // WiFiAdapter::AvailableNetworksChanged += ref new WiFiUpdateHandler(this, &Status::refresh_wifiinfo);

        this->update_timestamp();
        this->update_powerinfo();
        this->update_wifiinfo(nullptr);
        this->update_sdinfo();
        this->update_ipinfo();
    }

private:
    void refresh_powerinfo(Battery^ sender, Platform::Object^ e) {
        this->update_powerinfo();
    }

    void refresh_wifiinfo(WiFiAdapter^ sender, Platform::Object^ e) {
        this->update_wifiinfo(sender);
    }

private:
    Platform::String^ timestamp;
    Platform::String^ powercapacity;
    Platform::String^ wifi_strength;
    Platform::String^ storage;
    Platform::String^ ipv4;
    
private:
    Statusbarlet* master; // this will be destructed by master Control;

    friend class WarGrey::SCADA::Statusbarlet;
};

/*************************************************************************************************/
static Status^ statusbar = nullptr;
static CanvasTextFormat^ status_font = nullptr;
static float status_prefix_width = 0.0F;
static float status_height = 0.0F;

static void initialize_status_font() {
	if (status_font == nullptr) {
		status_font = make_text_format();

		TextExtent ts = get_text_extent(speak("plclabel"), status_font);
		status_height = ts.height;
		status_prefix_width = ts.width;
	}
}

Statusbarlet::Statusbarlet(Platform::String^ caption, Platform::String^ plc, IModbusConfirmation* console, ISyslogReceiver* uirecv) {
	auto logger = new Syslog(Log::Debug, caption, default_logger());
	
	if (uirecv != nullptr) {
		logger->append_log_receiver(uirecv);
	}

	this->caption = caption;
	this->client = new ModbusClient(logger, plc, console);
}

Statusbarlet::~Statusbarlet() {
	if (this->client != nullptr) {
		delete this->client;
	}
}

void Statusbarlet::load() {
	initialize_status_font();

    if (statusbar == nullptr) {
        statusbar = ref new Status(this);
    }
}

void Statusbarlet::fill_extent(float x, float y, float* width, float* height) {
	if ((this->info != nullptr) && (width != nullptr)) {
		float actual_width;

		this->info->master->fill_actual_extent(&actual_width, nullptr);
		(*width) = actual_width - x;
	}

	SET_BOX(height, status_height);
}

void Statusbarlet::update(long long count, long long interval, long long uptime, bool is_slow) {
    statusbar->update_timestamp();
}

void Statusbarlet::draw(CanvasDrawingSession^ ds, float x, float y, float Width, float Height) {
    auto timestamp = ref new CanvasTextLayout(ds, statusbar->timestamp, status_font, 0.0f, 0.0f);
    float width = Width / 7.0F;
	float timestamp_xoff = (Width - timestamp->LayoutBounds.Width);

    ds->DrawText(speak(this->caption),     x + width * 0.0F,   y, Colors::Yellow, status_font);
    ds->DrawText(statusbar->ipv4,          x + width * 1.0F,   y, Colors::White,  status_font);
    ds->DrawText(statusbar->powercapacity, x + width * 3.0F,   y, Colors::Green,  status_font);
    ds->DrawText(statusbar->wifi_strength, x + width * 4.0F,   y, Colors::Yellow, status_font);
    ds->DrawText(statusbar->storage,       x + width * 5.0F,   y, Colors::Yellow, status_font);
    ds->DrawTextLayout(timestamp,          x + timestamp_xoff, y, Colors::Yellow);

    { // highlight PLC Status
        auto plc_x = x + width * 2.0F;
        ds->DrawText(speak("plclabel"), plc_x, y, Colors::Yellow, status_font);
        if (this->client->connected()) {
            ds->DrawText(this->client->device_hostname(), plc_x + status_prefix_width, y, Colors::Green, status_font);
        } else {
			static Platform::String^ dots[] = { "", ".", "..", "..." , "...." , "....." , "......" };
			static unsigned int retry_count = 0;
			int idx = (retry_count ++) % (sizeof(dots) / sizeof(Platform::String^));

			ds->DrawText(speak("connecting") + dots[idx], plc_x + status_prefix_width, y, Colors::Red, status_font);
        }
    }
}

/*************************************************************************************************/
static ICanvasBrush^ status_colors[static_cast<unsigned int>(Log::None) + 1];

void Statuslinelet::load() {
	initialize_status_font();

	if (status_colors[0] == nullptr) {
		status_colors[static_cast<unsigned int>(Log::Debug)] = make_solid_brush(Colors::Gray);
		status_colors[static_cast<unsigned int>(Log::Info)] = make_solid_brush(Colors::Green);
		status_colors[static_cast<unsigned int>(Log::Notice)] = make_solid_brush(Colors::GreenYellow);
		status_colors[static_cast<unsigned int>(Log::Warning)] = make_solid_brush(Colors::Yellow);
		status_colors[static_cast<unsigned int>(Log::Error)] = make_solid_brush(Colors::Red);
		status_colors[static_cast<unsigned int>(Log::Critical)] = make_solid_brush(Colors::Crimson);
		status_colors[static_cast<unsigned int>(Log::Alert)] = make_solid_brush(Colors::Firebrick);
		status_colors[static_cast<unsigned int>(Log::Panic)] = make_solid_brush(Colors::Firebrick);
		status_colors[static_cast<unsigned int>(Log::None)] = make_solid_brush(Colors::GhostWhite);
	}

	this->status = make_text_layout("", status_font);
}

void Statuslinelet::fill_extent(float x, float y, float* width, float* height) {
	if ((this->info != nullptr) && (width != nullptr)) {
		float actual_width;

		this->info->master->fill_actual_extent(&actual_width, nullptr);
		(*width) = actual_width - x;
	}

	SET_BOX(height, status_height);
}

void Statuslinelet::draw(CanvasDrawingSession^ ds, float x, float y, float Width, float Height) {
	ds->DrawTextLayout(this->status, x, y, status_colors[static_cast<unsigned int>(Log::None)]);
}

void Statuslinelet::on_log_message(Log level, Platform::String^ message, SyslogMetainfo& data, Platform::String^ topic) {
	Platform::String^ status_message = "[" + level.ToString() + "] " + message;
	ICanvasBrush^ color = status_colors[static_cast<unsigned int>(level)];

	this->status = make_text_layout(status_message, status_font);
	if (color != nullptr) {
		this->status->SetBrush(0, status_message->Length(), color);
	}
}
