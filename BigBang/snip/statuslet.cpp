#include <algorithm>
#include <ppltasks.h>
#include <shared_mutex>

#include "text.hpp"
#include "paint.hpp"
#include "time.hpp"
#include "tongue.hpp"
#include "planet.hpp"
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

static CanvasTextFormat^ status_font = nullptr;
static float status_prefix_width = 0.0F;
static float status_height = 0.0F;

// delegate only accepts C++/CX class
private ref class Status sealed {
internal:
	Status() {
		this->clock = ref new DispatcherTimer();

		Battery::AggregateBattery->ReportUpdated += ref new BatteryUpdateHandler(this, &Status::refresh_powerinfo);
		// WiFiAdapter::AvailableNetworksChanged += ref new WiFiUpdateHandler(this, &Status::refresh_wifiinfo);

		this->update_powerinfo();
		this->update_wifiinfo();
		this->update_sdinfo();
		this->update_ipinfo();

		this->clock->Tick += ref new EventHandler<Object^>(this, &Status::refresh_timestamp);
		this->clock->Interval = this->update_timestamp();
		this->clock->Start();
	}

public:
    TimeSpan update_timestamp() {
        int l00ns;

        this->timestamp = make_text_layout(update_nowstamp(false, &l00ns), status_font);
		
        return TimeSpan{ 10000000LL - l00ns };
    }

    void update_powerinfo() {
        auto info = Battery::AggregateBattery->GetReport();
		auto maybe_remaining = info->RemainingCapacityInMilliwattHours;
		Platform::String^ power = nullptr;

		if (maybe_remaining == nullptr) {
			power = speak("powerlabel") + "100%";
		} else {
			auto remaining = float(info->RemainingCapacityInMilliwattHours->Value);
			auto full = float(info->FullChargeCapacityInMilliwattHours->Value);

			power = speak("powerlabel") + round((remaining / full) * 100.0F).ToString() + "%";
		}

		this->powercapacity = make_text_layout(power, status_font);
    }

    void update_wifiinfo() {
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

        this->wifi_strength = make_text_layout(speak("wifilabel") + signal, status_font);
    }

    void update_sdinfo() {
        this->storage = make_text_layout(speak("sdlabel") + L"0MB", status_font);
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

		this->ipv4 = make_text_layout(speak("ipv4label") + ipv4, status_font);
    }

public:
	void enter_critical_section() {
		this->section.lock();
	}

	void leave_critical_section() {
		this->section.unlock();
	}

	void enter_shared_section() {
		this->section.lock_shared();
	}

	void leave_shared_section() {
		this->section.unlock_shared();
	}

private:
	void refresh_timestamp(Platform::Object^ sender, Platform::Object^ e) {
		this->enter_critical_section();
		this->clock->Interval = this->update_timestamp();
		this->leave_critical_section();
	}
	
	void refresh_powerinfo(Battery^ sender, Platform::Object^ e) {
		this->enter_critical_section();
		this->update_powerinfo();
		this->leave_critical_section();
    }

    void refresh_wifiinfo(WiFiAdapter^ sender, Platform::Object^ e) {
		this->enter_critical_section();
		this->update_wifiinfo();
		this->leave_critical_section();
    }

private:
    CanvasTextLayout^ timestamp;
	CanvasTextLayout^ powercapacity;
	CanvasTextLayout^ wifi_strength;
	CanvasTextLayout^ storage;
	CanvasTextLayout^ ipv4;
    
private:
	DispatcherTimer^ clock;
	std::shared_mutex section;

private:
	~Status() {
		this->clock->Stop();
	}

    friend class WarGrey::SCADA::Statusbarlet;
};

/*************************************************************************************************/
static Status^ statusbar = nullptr;

static void initialize_status_font() {
	if (status_font == nullptr) {
		status_font = make_text_format();

		TextExtent ts = get_text_extent(speak("plclabel"), status_font);
		status_height = ts.height;
		status_prefix_width = ts.width;
	}
}

Statusbarlet::Statusbarlet(Platform::String^ caption, Platform::String^ plc
	, IModbusConfirmation* workbench, ISyslogReceiver* uirecv) {
	auto logger = new Syslog(Log::Debug, caption, default_logger());
	
	if (uirecv != nullptr) {
		logger->append_log_receiver(uirecv);
	}

	initialize_status_font();
	this->caption = make_text_layout(speak("RR") + " " + speak(caption), status_font);
	this->client = new ModbusClient(logger, plc, workbench);
	workbench->fill_application_input_register_interval(&this->start_address, &this->end_address, &this->quantity);
}

Statusbarlet::~Statusbarlet() {
	if (this->client != nullptr) {
		delete this->client;
	}
}

void Statusbarlet::construct() {
	if (statusbar == nullptr) {
		statusbar = ref new Status();
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
	if (this->client->connected()) {
		uint16 address = this->start_address;

		while (address < this->end_address) {
			uint16 q = min(this->quantity, this->end_address - address);
			this->client->read_input_registers(address, q);
			address += q;
		}
	}
}

void Statusbarlet::draw(CanvasDrawingSession^ ds, float x, float y, float Width, float Height) {
    float width = Width / 7.0F;
	
	ds->FillRectangle(x, y, Width, Height, system_background_brush());

	statusbar->enter_shared_section();
	float timestamp_xoff = (Width - statusbar->timestamp->LayoutBounds.Width);
	ds->DrawTextLayout(this->caption,            x + width * 0.0F,   y, Colors::Yellow);
    ds->DrawTextLayout(statusbar->ipv4,          x + width * 1.0F,   y, Colors::White);
    ds->DrawTextLayout(statusbar->powercapacity, x + width * 3.0F,   y, Colors::Green);
    ds->DrawTextLayout(statusbar->wifi_strength, x + width * 4.0F,   y, Colors::Yellow);
    ds->DrawTextLayout(statusbar->storage,       x + width * 5.0F,   y, Colors::Yellow);
    ds->DrawTextLayout(statusbar->timestamp,     x + timestamp_xoff, y, Colors::Yellow);
	statusbar->leave_shared_section();

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

IModbusClient* Statusbarlet::get_modbus_client() {
	return this->client;
}

/*************************************************************************************************/
static ICanvasBrush^ status_colors[static_cast<unsigned int>(Log::None) + 1];
static ICanvasBrush^ status_nolog_color = nullptr;

void Statuslinelet::construct() {
	initialize_status_font();

	if (status_nolog_color == nullptr) {
		status_nolog_color = make_solid_brush(Colors::GhostWhite);
		status_colors[static_cast<unsigned int>(Log::Debug)] = make_solid_brush(Colors::Gray);
		status_colors[static_cast<unsigned int>(Log::Info)] = make_solid_brush(Colors::Green);
		status_colors[static_cast<unsigned int>(Log::Notice)] = make_solid_brush(Colors::GreenYellow);
		status_colors[static_cast<unsigned int>(Log::Warning)] = make_solid_brush(Colors::Yellow);
		status_colors[static_cast<unsigned int>(Log::Error)] = make_solid_brush(Colors::Red);
		status_colors[static_cast<unsigned int>(Log::Critical)] = make_solid_brush(Colors::Crimson);
		status_colors[static_cast<unsigned int>(Log::Alert)] = make_solid_brush(Colors::Firebrick);
		status_colors[static_cast<unsigned int>(Log::Panic)] = make_solid_brush(Colors::Firebrick);
	}

	this->set_message("");
}

void Statuslinelet::set_message(Platform::String^ message, Log level) {
	auto lcolor = status_colors[static_cast<unsigned int>(level)]; 
	this->color = ((lcolor == nullptr) ? status_nolog_color : lcolor);

	if (this->info == nullptr) {
		this->status = make_text_layout(message, status_font);
	} else {
		this->info->master->enter_critical_section();
		this->status = make_text_layout(message, status_font);
		this->info->master->leave_critical_section();
	}
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
	ds->FillRectangle(x, y, Width, Height, system_background_brush());
	ds->DrawTextLayout(this->status, x, y, this->color);
}

void Statuslinelet::on_log_message(Log level, Platform::String^ message, SyslogMetainfo& data, Platform::String^ topic) {
	this->set_message("[" + level.ToString() + "] " + message, level);
}
