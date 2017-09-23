#include <cmath>
#include <ppltasks.h>

#include "ui.hpp"
#include "text.hpp"
#include "time.hpp"
#include "tongue.hpp"
#include "pasteboard.hxx"
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

typedef TypedEventHandler<Battery^, Platform::Object^> BatteryUpdateHandler;
typedef TypedEventHandler<WiFiAdapter^, Platform::Object^> WiFiUpdateHandler;

#define REFRESH(self) if (self->master->info != nullptr) { self->master->info->master->refresh(); }

// delegate only accepts C++/CX
ref class Status sealed {
public:
    TimeSpan update_timestamp() {
        int l00ns;

        this->timestamp = update_nowstamp(false, &l00ns);
        REFRESH(this);

        return TimeSpan{ 10000000 - l00ns };
    }

    void update_powerinfo() {
        auto info = Battery::AggregateBattery->GetReport();
        auto remaining = float(info->RemainingCapacityInMilliwattHours->Value);
        auto full = float(info->FullChargeCapacityInMilliwattHours->Value);

        this->powercapacity = speak("powerlabel") + std::round((remaining / full) * 100.0F).ToString() + "%";
        REFRESH(this);
    }

    void update_wifiinfo(WiFiAdapter^ info) {
        auto nics = NetworkInformation::GetConnectionProfiles();
        Platform::String^ signal = speak("nowifi");

        for (unsigned int i = 0; i < nics->Size; ++i) {
            auto nic = nics->GetAt(i);
            if (nic->IsWlanConnectionProfile) {
                auto signal_bar = nic->GetSignalBars()->Value;
                signal = std::round(signal_bar / 5 * 100).ToString() + L"%";
                break;
            }
        }

        this->wifi_strength = speak("wifilabel") + signal;
        REFRESH(this);
    }

    void update_sdinfo() {
        this->storage = speak("sdlabel") + L"0MB";
        REFRESH(this);
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
        REFRESH(this);
    }

internal:
    Status(Statuslet* master) {
        this->master = master;

        Battery::AggregateBattery->ReportUpdated += ref new BatteryUpdateHandler(this, &Status::refresh_powerinfo);
        // WiFiAdapter::AvailableNetworksChanged += ref new WiFiUpdateHandler(this, &Status::refresh_wifiinfo);
        this->timer = gui_timer(1000, ref new ObjectHandler(this, &Status::refresh_timeinfo));

        this->update_timestamp();
        this->update_powerinfo();
        this->update_wifiinfo(nullptr);
        this->update_sdinfo();
        this->update_ipinfo();
    }

private:
    void refresh_timeinfo(Platform::Object^ sender, Platform::Object^ e) {
        this->timer->Interval = this->update_timestamp();
    }

    void refresh_powerinfo(Battery^ sender, Platform::Object^ e) {
        this->update_powerinfo();
    }

    void refresh_wifiinfo(WiFiAdapter^ sender, Platform::Object^ e) {
        this->update_wifiinfo(sender);
    }

private:
    DispatcherTimer^ timer;
    Platform::String^ timestamp;
    Platform::String^ powercapacity;
    Platform::String^ wifi_strength;
    Platform::String^ storage;
    Platform::String^ ipv4;
    
private:
    Statuslet* master; // this will be destructed by master Pasteboard;

    friend class WarGrey::SCADA::Statuslet;
};

/*************************************************************************************************/
static Status^ statusbar = nullptr;
static float status_prefix_width = 0.0F;
static float status_height = 0.0F;

Statuslet::Statuslet(Platform::String^ caption) {
    this->caption = caption;
    this->plc_connected = false;
    this->label_font = make_text_format();

    // TODO: is it safe?
    statusbar = ref new Status(this);
}

void Statuslet::fill_extent(float x, float y, float* w, float* h, float* b, float* t, float* l, float* r) {
    if (statusbar->master != nullptr) {
        if (status_height == 0.0F) {
            TextExtent ts = get_text_extent(speak("plclabel"), label_font);
            status_height = ts.height;

            // TODO: Win2D eats suffix spaces.
            TextExtent sp = get_text_extent("o", label_font);
            status_prefix_width = ts.width + sp.width;
        }

        SET_BOX(w, info->master->actual_layer_width - x);
        SET_BOX(h, status_height);
        SET_BOXES(b, t, 0.0F);
        SET_BOXES(l, r, 0.0F);
    }
}

void Statuslet::draw(CanvasDrawingSession^ ds, float x, float y, float Width, float Height) {
    auto ipv4 = ref new CanvasTextLayout(ds, statusbar->ipv4, this->label_font, 0.0f, 0.0f);
    auto width = Width / 7.0F;

    ds->DrawText(this->caption,            x + width * 0.0F, y, Colors::Yellow, this->label_font);
    ds->DrawText(statusbar->timestamp,     x + width * 1.0F, y, Colors::Yellow, this->label_font);
    ds->DrawText(statusbar->powercapacity, x + width * 2.0F, y, Colors::Green, this->label_font);
    ds->DrawText(statusbar->wifi_strength, x + width * 3.0F, y, Colors::Yellow, this->label_font);
    ds->DrawText(statusbar->storage,       x + width * 5.0F, y, Colors::Yellow, this->label_font);
    ds->DrawTextLayout(ipv4, Width - ipv4->LayoutBounds.Width, y, Colors::White);

    { // highlight PLC Status
        auto plc_x = x + width * 4.0F;
        ds->DrawText(speak("plclabel"), plc_x, y, Colors::Yellow, this->label_font);
        if (this->plc_connected) {
            ds->DrawText(speak("connected"), plc_x + status_prefix_width, y, Colors::Green, this->label_font);
        } else {
            ds->DrawText(speak("disconnected"), plc_x + status_prefix_width, y, Colors::Red, this->label_font);
        }
    }
}
