#include <algorithm>
#include <shared_mutex>

#include "graphlet/statuslet.hpp"

#include "text.hpp"
#include "paint.hpp"
#include "system.hpp"
#include "tongue.hpp"
#include "planet.hpp"
#include "brushes.hxx"

using namespace WarGrey::SCADA;

using namespace Microsoft::Graphics::Canvas;
using namespace Microsoft::Graphics::Canvas::Text;
using namespace Microsoft::Graphics::Canvas::Brushes;

static CanvasTextFormat^ status_font = nullptr;
static float status_prefix_width = 0.0F;
static float status_height = 0.0F;

private class SingletonStatus final : public ISystemStatusListener {
	friend class WarGrey::SCADA::Statusbarlet;
public:
	void on_timestamp_changed(Platform::String^ timestamp) override {
		this->enter_critical_section();
		this->clock = make_text_layout(timestamp, status_font);
		this->leave_critical_section();
    }

    void on_battery_capacity_changed(float capacity) override {
		Platform::String^ label = speak(":power:");
		Platform::String^ percentage = (round(capacity * 100.0F).ToString() + "%");

		this->enter_critical_section();
		this->battery = make_text_layout(label + percentage, status_font);
		this->leave_critical_section();
	}

    void on_wifi_signal_strength_changed(char strength) override {
		float percentage = std::roundf(float(strength) * 100.0F / 5.0F);
		Platform::String^ label = speak(":wifi:");
        Platform::String^ signal = ((strength < 0) ? speak(":nowifi:") : (percentage.ToString() + "%"));

		this->enter_critical_section();
        this->wifi = make_text_layout(label + signal, status_font);
		this->leave_critical_section();
	}

    void on_available_storage_changed(long long bytes) override {
		Platform::String^ label = speak(":sd:");
		Platform::String^ size = bytes.ToString();

		this->enter_critical_section();
        this->storage = make_text_layout(label + size, status_font);
		this->leave_critical_section();
	}

    void on_ipv4_address_changed(Platform::String^ ipv4) override {
		Platform::String^ label = speak(":ipv4:");
		Platform::String^ ip = ((ipv4 == nullptr) ? speak(":noipv4:") : ipv4);

		this->enter_critical_section();
		this->ipv4 = make_text_layout(label + ip, status_font);
		this->leave_critical_section();
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
    CanvasTextLayout^ clock;
	CanvasTextLayout^ battery;
	CanvasTextLayout^ wifi;
	CanvasTextLayout^ storage;
	CanvasTextLayout^ ipv4;
    
private:
	std::shared_mutex section;
};

/*************************************************************************************************/
static SingletonStatus* statusbar = nullptr;

static void initialize_status_font() {
	if (status_font == nullptr) {
		status_font = make_bold_text_format("Microsoft YaHei", 12.0F);
		
		TextExtent ts = get_text_extent(speak(":plc:"), status_font);
		status_height = ts.height * 1.2F;
		status_prefix_width = ts.width;
	}
}

float WarGrey::SCADA::statusbar_height() {
	initialize_status_font();

	return status_height;
}

/*************************************************************************************************/
Statusbarlet::Statusbarlet(Platform::String^ caption, IPLCMaster* device) {
	initialize_status_font();
	this->device = device;
	this->caption = make_text_layout(speak(caption), status_font);
}

void Statusbarlet::construct() {
	if (statusbar == nullptr) {
		statusbar = new SingletonStatus();
		register_system_status_listener(statusbar);
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

void Statusbarlet::update(long long count, long long interval, long long uptime) {
	if ((this->device != nullptr) && (this->device->connected())) {
		this->device->send_scheduled_request(count, interval, uptime);
	}
}

void Statusbarlet::draw(CanvasDrawingSession^ ds, float x, float y, float Width, float Height) {
    float width = Width / 7.0F;
	float context_y = y + (status_height - this->caption->LayoutBounds.Height) * 0.5F;
	
	ds->FillRectangle(x, y, Width, Height, Colours::Background);

	statusbar->enter_shared_section();
	float lastone_xoff = (Width - statusbar->ipv4->LayoutBounds.Width);
	ds->DrawTextLayout(this->caption,      x + width * 0.0F, context_y, Colours::Chocolate);
	ds->DrawTextLayout(statusbar->clock,   x + width * 1.0F, context_y, Colours::Foreground);
	ds->DrawTextLayout(statusbar->battery, x + width * 2.0F, context_y, Colours::Green);
	ds->DrawTextLayout(statusbar->wifi,    x + width * 3.0F, context_y, Colours::Yellow);
	ds->DrawTextLayout(statusbar->storage, x + width * 5.0F, context_y, Colours::Yellow);
    ds->DrawTextLayout(statusbar->ipv4,    x + lastone_xoff, context_y, Colours::Yellow);
	statusbar->leave_shared_section();

	{ // highlight PLC Status
		float plc_x = x + width * 4.0F;
		
		ds->DrawText(speak(":plc:"), plc_x, context_y, Colours::Yellow, status_font);

		if (this->device == nullptr) {
			if (this->device_name == nullptr) {
				this->device_name = make_text_layout(speak(":offline:"), status_font);
			}

			ds->DrawTextLayout(this->device_name, plc_x + status_prefix_width, context_y, Colours::Red);
		} else if (this->device->connected()) {
			if (this->device_name == nullptr) {
				this->device_name = make_text_layout(this->device->device_hostname(), status_font);
			}

			ds->DrawTextLayout(this->device_name, plc_x + status_prefix_width, context_y, Colours::Green);
		} else {
			static Platform::String^ dots[] = { "", ".", "..", "..." , "...." , "....." , "......" };
			static unsigned int retry_count = 0;
			int idx = (retry_count++) % (sizeof(dots) / sizeof(Platform::String^));
			
			ds->DrawText(speak(":connecting:") + dots[idx], plc_x + status_prefix_width, context_y, Colours::Red, status_font);
			this->device_name = nullptr;
		}
	}
}

/*************************************************************************************************/
static ICanvasBrush^ status_colors[static_cast<unsigned int>(Log::None) + 1];
static ICanvasBrush^ status_nolog_color = nullptr;

void Statuslinelet::construct() {
	initialize_status_font();

	if (status_nolog_color == nullptr) {
		status_nolog_color = Colours::GhostWhite;
		status_colors[static_cast<unsigned int>(Log::Debug)] = Colours::Silver;
		status_colors[static_cast<unsigned int>(Log::Info)] = Colours::Green;
		status_colors[static_cast<unsigned int>(Log::Notice)] = Colours::GreenYellow;
		status_colors[static_cast<unsigned int>(Log::Warning)] = Colours::Yellow;
		status_colors[static_cast<unsigned int>(Log::Error)] = Colours::Red;
		status_colors[static_cast<unsigned int>(Log::Critical)] = Colours::Crimson;
		status_colors[static_cast<unsigned int>(Log::Alarm)] = Colours::Firebrick;
		status_colors[static_cast<unsigned int>(Log::Panic)] = Colours::Firebrick;
	}

	this->set_message("");
}

void Statuslinelet::set_message(Platform::String^ message, Log level) {
	auto lcolor = status_colors[static_cast<unsigned int>(level)]; 
	this->color = ((lcolor == nullptr) ? status_nolog_color : lcolor);

	this->section.lock();
	this->status = make_text_layout(message, status_font);
	this->section.unlock();
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
	float content_y = y + (status_height - this->status->LayoutBounds.Height) * 0.5F;

	ds->FillRectangle(x, y, Width, Height, Colours::Background);

	this->section.lock_shared();
	ds->DrawTextLayout(this->status, x, content_y, this->color);
	this->section.unlock_shared();
}

void Statuslinelet::on_log_message(Log level, Platform::String^ message, SyslogMetainfo& data, Platform::String^ topic) {
	this->set_message("[" + level.ToString() + "] " + message, level);
}
