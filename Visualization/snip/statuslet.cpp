#include <cmath>
#include <ppltasks.h>

#include "ui.hpp"
#include "time.hpp"
#include "rsyslog.hpp"
#include "pasteboard.hxx"
#include "snip/statuslet.hpp"

using namespace WarGrey::SCADA;

using namespace Concurrency;
using namespace Windows::UI;
using namespace Windows::UI::Xaml;

using namespace Windows::Foundation;
using namespace Windows::Foundation::Collections;

using namespace Windows::Devices::Power;
using namespace Windows::Devices::WiFi;

using namespace Windows::Networking;
using namespace Windows::Networking::Connectivity;

using namespace Microsoft::Graphics::Canvas;
using namespace Microsoft::Graphics::Canvas::Text;

typedef TypedEventHandler<Battery^, Platform::Object^> BatteryUpdateHandler;
typedef TypedEventHandler<WiFiAdapter^, Platform::Object^> WiFiUpdateHandler;


static Platform::String^ plc_prefix = L"PLC 状态:"; // do not add space after ':'
static Platform::String^ power_prefix = L"电量: ";
static Platform::String^ wifi_prefix = L"WiFi: ";
static Platform::String^ sd_prefix = L"SD 容量: ";
static Platform::String^ ip_prefix = L"本机 IP: ";

// delegate only accepts C++/CX
ref class Status sealed {
public:
    TimeSpan update_timestamp() {
        int l00ns;

        this->timestamp = update_nowstamp(false, &l00ns);
        if (this->master != nullptr) {
            this->master->refresh();
        }

        return TimeSpan{ 10000000 - l00ns };
    }

    void update_powerinfo() {
        auto info = Battery::AggregateBattery->GetReport();
        auto remaining = float(info->RemainingCapacityInMilliwattHours->Value);
        auto full = float(info->FullChargeCapacityInMilliwattHours->Value);

        this->powercapacity = power_prefix + std::round((remaining / full) * 100.0F).ToString() + "%";
        
        if (this->master != nullptr) {
            this->master->refresh();
        }
    }

    void update_wifiinfo(WiFiAdapter^ info) {
        if (this->master != nullptr) {
            this->master->refresh();
        }
    }

    void update_nicinfo() {
        auto nics = NetworkInformation::GetConnectionProfiles();
        auto names = NetworkInformation::GetHostNames();
        Platform::String^ ipv4 = L"0.0.0.0";
        Platform::String^ signal = L"No";

        for (unsigned int i = 0; i < nics->Size; ++i) {
            auto nic = nics->GetAt(i);
            if (nic->IsWlanConnectionProfile) {
                auto signal_bar = nic->GetSignalBars()->Value;
                signal = std::round(signal_bar / 5 * 100).ToString() + L"%";
                break;
            }
        }

        for (unsigned int i = 0; i < names->Size; ++i) {
            auto host = names->GetAt(i);
            if (host->Type == HostNameType::Ipv4) {
                ipv4 = host->RawName;
                break;
            }
        }
        
        this->wifi_strength = wifi_prefix + signal;
        this->localhost = ip_prefix + ipv4;
    }

    void update_sdinfo() {
        this->storage = sd_prefix + L"0MB";
    }

internal:
    Status() {
        Battery::AggregateBattery->ReportUpdated += ref new BatteryUpdateHandler(this, &Status::refresh_powerinfo);
        // WiFiAdapter::AvailableNetworksChanged += ref new WiFiUpdateHandler(this, &Status::refresh_wifiinfo);
        this->timer = gui_timer(1000, ref new ObjectHandler(this, &Status::refresh_timeinfo));
        
        this->update_timestamp();
        this->update_powerinfo();
        this->update_sdinfo();
        this->update_nicinfo();
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
    Platform::String^ localhost;
    
private:
    Pasteboard^ master;

    friend class WarGrey::SCADA::Statuslet;
};

/*************************************************************************************************/
static Status^ statusbar = nullptr;
static float status_height = 0.0F;

Statuslet::Statuslet(Platform::String^ caption) {
    this->caption = caption;
    this->plc_connected = false;
    this->font = ref new CanvasTextFormat();

    this->font->WordWrapping = CanvasWordWrapping::NoWrap;
    this->font->FontSize = 12;

    if (statusbar == nullptr) {
        statusbar = ref new Status();
    }
}

void Statuslet::on_attach_to(Pasteboard^ master) {
    statusbar->master = master;
}

void Statuslet::fill_extent(float x, float y, float* w, float* h, float* b, float* t, float* l, float* r) {
    if (statusbar->master != nullptr) {
        if (status_height == 0.0F) {
            TextExtent ts = get_text_extent(plc_prefix, font);
            status_height = ts.height;
        }

        SET_BOX(w, statusbar->master->actual_layer_width - x);
        SET_BOX(h, status_height);
        SET_BOXES(b, t, 0.0F);
        SET_BOXES(l, r, 0.0F);
    }
}

void Statuslet::draw(CanvasDrawingSession^ ds, float x, float y, float Width, float Height) {
    auto localhost = ref new CanvasTextLayout(ds, statusbar->localhost, font, 0.0f, 0.0f);
    auto plc_label = ref new CanvasTextLayout(ds, plc_prefix, font, 0.0F, 0.0F);
    auto width = Width / 7.0F;
    auto plc_x = x + width * 4.0F;

    ds->DrawTextLayout(localhost, Width - localhost->LayoutBounds.Width, y, Colors::White);
    ds->DrawTextLayout(plc_label, plc_x, y, Colors::Yellow);
    if (this->plc_connected) {
        ds->DrawText(L" 连接", plc_x + plc_label->LayoutBounds.Width, y, Colors::Green, font);
    } else {
        ds->DrawText(L" 断开", plc_x + plc_label->LayoutBounds.Width, y, Colors::Red, font);
    }

    ds->DrawText(this->caption,            x + width * 0.0F, y, Colors::Yellow, font);
    ds->DrawText(statusbar->timestamp,     x + width * 1.0F, y, Colors::Yellow, font);
    ds->DrawText(statusbar->powercapacity, x + width * 2.0F, y, Colors::Green, font);
    ds->DrawText(statusbar->wifi_strength, x + width * 3.0F, y, Colors::Yellow, font);
    ds->DrawText(statusbar->storage,       x + width * 5.0F, y, Colors::Yellow, font);
}
