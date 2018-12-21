#include <algorithm>
#include <shared_mutex>
#include <map>

#include "graphlet/statuslet.hpp"

#include "text.hpp"
#include "polar.hpp"
#include "paint.hpp"
#include "string.hpp"
#include "system.hpp"
#include "tongue.hpp"
#include "planet.hpp"
#include "brushes.hxx"

using namespace WarGrey::SCADA;

using namespace Windows::System;

using namespace Microsoft::Graphics::Canvas;
using namespace Microsoft::Graphics::Canvas::Text;
using namespace Microsoft::Graphics::Canvas::Brushes;

static Platform::String^ tongue_scope = "status";

static CanvasTextFormat^ status_font = nullptr;
static float status_prefix_width = 0.0F;
static float status_height = 0.0F;

private class SingletonStatus final : public ISystemStatusListener {
	friend class WarGrey::SCADA::Statusbarlet;
public:
	void on_timestamp_changed(Platform::String^ timestamp) override {
		this->enter_critical_section();
		this->clock = make_text_layout(timestamp, status_font);
		this->updated = true;
		this->leave_critical_section();
    }

    void on_battery_capacity_changed(float capacity) override {
		Platform::String^ label = speak("power", tongue_scope);
		Platform::String^ percentage = (round(capacity * 100.0F).ToString() + "%");

		this->enter_critical_section();
		this->battery = make_text_layout(label + percentage, status_font);
		this->updated = true;
		this->leave_critical_section();
	}

	void on_brightness_changed(double brightness) override {
		Platform::String^ label = speak("brightness", tongue_scope);
		Platform::String^ percentage = (round(brightness * 100.0F).ToString() + "%");

		this->enter_critical_section();
		this->brightness = make_text_layout(label + percentage, status_font);
		this->updated = true;
		this->leave_critical_section();
	}

    void on_wifi_signal_strength_changed(Platform::String^ ssid, char strength) override {
		float percentage = std::roundf(float(strength) * 100.0F / 5.0F);
		Platform::String^ label = speak("wifi", tongue_scope);
        Platform::String^ signal = ((ssid == nullptr) ? speak("nowifi", tongue_scope) : (ssid + " " + percentage.ToString() + "%"));

		this->enter_critical_section();
        this->wifi = make_text_layout(label + signal, status_font);
		this->updated = true;
		this->leave_critical_section();
	}

    void on_available_storage_changed(unsigned long long free_bytes, unsigned long long total_bytes) override {
		Platform::String^ label = speak("storage", tongue_scope);
		Platform::String^ percentage = flstring(double(free_bytes) / double(total_bytes) * 100.0, 1);
		Platform::String^ free = sstring(free_bytes, 1);

		this->enter_critical_section();
        this->storage = make_text_layout(label + free + "(" + percentage + "%)", status_font);
		this->updated = true;
		this->leave_critical_section();
	}

    void on_ipv4_address_changed(Platform::String^ ipv4) override {
		Platform::String^ label = speak("ipv4", tongue_scope);
		Platform::String^ ip = ((ipv4 == nullptr) ? speak("noipv4", tongue_scope) : ipv4);

		this->enter_critical_section();
		this->ipv4 = make_text_layout(label + ip, status_font);
		this->updated = true;
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

public:
	bool needs_update() {
		bool has_new_info = false;

		this->enter_shared_section();
		has_new_info = this->updated;
		this->updated = false;
		this->leave_shared_section();

		return has_new_info;
	}

private:
    CanvasTextLayout^ clock;
	CanvasTextLayout^ battery;
	CanvasTextLayout^ brightness;
	CanvasTextLayout^ wifi;
	CanvasTextLayout^ storage;
	CanvasTextLayout^ ipv4;
    
private:
	std::shared_mutex section;
	bool updated;
};

/*************************************************************************************************/
static SingletonStatus* statusbar = nullptr;

static void initialize_status_font() {
	if (status_font == nullptr) {
		status_font = make_bold_text_format("Microsoft YaHei", 14.0F);
		
		TextExtent te = get_text_extent(speak("plc", tongue_scope), status_font);
		status_height = te.height * 1.2F;
		status_prefix_width = te.width;
	}
}

float WarGrey::SCADA::statusbar_height() {
	initialize_status_font();

	return status_height;
}

/*************************************************************************************************/
Statusbarlet::Statusbarlet(Platform::String^ caption, IPLCMaster* device) : device(device) {
	initialize_status_font();
	this->caption = make_text_layout(speak(caption), status_font);
}

void Statusbarlet::construct() {
	if (statusbar == nullptr) {
		statusbar = new SingletonStatus();
		register_system_status_listener(statusbar);
	}

	this->retry_icon_size = statusbar_height() * 0.618F;
	this->retry_icon = geometry_freeze(polar_arrowhead(this->retry_icon_size * 0.5F, 0.0));
}

void Statusbarlet::fill_extent(float x, float y, float* width, float* height) {
	SET_BOX(width, fmax(this->available_visible_width(x), 0.0F));
	SET_BOX(height, status_height);
}

void Statusbarlet::update(long long count, long long interval, long long uptime) {
	if (this->device != nullptr) {
		if (this->device->connected()) {
			this->device->send_scheduled_request(count, interval, uptime);
		} else {
			this->retry_step = count % 9;
			this->notify_updated();
		}
	}

	if (statusbar->needs_update()) {
		this->notify_updated();
	}
}

void Statusbarlet::draw(CanvasDrawingSession^ ds, float x, float y, float Width, float Height) {
    float width = Width / 9.0F;
	float context_y = y + (status_height - this->caption->LayoutBounds.Height) * 0.5F;
	
	ds->FillRectangle(x, y, Width, Height, Colours::Background);

	statusbar->enter_shared_section();
	float lastone_xoff = (Width - statusbar->ipv4->LayoutBounds.Width);
	ds->DrawTextLayout(this->caption, x + width * 0.0F, context_y, Colours::Chocolate);
	ds->DrawTextLayout(statusbar->clock, x + width * 1.0F, context_y, Colours::Foreground);
	ds->DrawTextLayout(statusbar->battery, x + width * 2.0F, context_y, Colours::Green);
	ds->DrawTextLayout(statusbar->brightness, x + width * 3.0F, context_y, Colours::LightBlue);
	ds->DrawTextLayout(statusbar->wifi, x + width * 5.0F, context_y, Colours::Yellow);
	ds->DrawTextLayout(statusbar->storage, x + width * 7.0F, context_y, Colours::YellowGreen);
	ds->DrawTextLayout(statusbar->ipv4, x + lastone_xoff, context_y, Colours::Yellow);
	statusbar->leave_shared_section();

	{ // draw App Memory Usage
		AppMemoryUsageLevel level;
		unsigned long long memory = system_memory_usage(&level);
		CanvasSolidColorBrush^ color = Colours::YellowGreen;

		switch (level) {
		case AppMemoryUsageLevel::OverLimit: color = Colours::Firebrick; break;
		case AppMemoryUsageLevel::High: color = Colours::Orange; break;
		case AppMemoryUsageLevel::Low: color = Colours::RoyalBlue; break;
		}

		ds->DrawText(speak("memory", tongue_scope) + ": " + sstring(memory, 2),
			x + width * 4.0F, context_y,
			color, status_font);
	}

	{ // draw PLC State
		float plc_x = x + width * 6.0F;

		ds->DrawText(speak("plc", tongue_scope), plc_x, context_y, Colours::Yellow, status_font);

		if (this->device == nullptr) {
			if (this->device_name == nullptr) {
				this->device_name = make_text_layout(speak("offline", tongue_scope), status_font);
			}

			ds->DrawTextLayout(this->device_name, plc_x + status_prefix_width, context_y, Colours::Red);
		} else if (this->device->connected()) {
			if ((this->device_name == nullptr) || (this->plc_mode != this->device->get_mode())) {
				this->plc_mode = this->device->get_mode();
				this->device_name = make_text_layout(this->device->device_hostname(), status_font);
			}

			if (this->device->authorized()) {
				ds->DrawTextLayout(this->device_name, plc_x + status_prefix_width, context_y, Colours::Green);
			} else {
				ds->DrawTextLayout(this->device_name, plc_x + status_prefix_width, context_y, Colours::Cyan);
			}
		} else {
			float icon_cx = plc_x + status_prefix_width + this->retry_icon_size * 0.5F;
			float icon_cy = y + status_height * 0.5F;

			for (unsigned int i = 0; i < this->retry_step; i++) {
				ds->DrawCachedGeometry(this->retry_icon,
					icon_cx + this->retry_icon_size * float(i), icon_cy,
					Colours::Orange);
			}
		}
	}
}

/*************************************************************************************************/
static std::map<Log, ICanvasBrush^> status_colors;

Statuslinelet::Statuslinelet(Log level, unsigned int lines) : ISyslogReceiver(level), lines(lines) {}

void Statuslinelet::construct() {
	initialize_status_font();

	if (status_colors.size() == 0) {
		status_colors[Log::_] = Colours::GhostWhite;
		status_colors[Log::Debug] = Colours::Silver;
		status_colors[Log::Info] = Colours::Green;
		status_colors[Log::Notice] = Colours::GreenYellow;
		status_colors[Log::Warning] = Colours::Yellow;
		status_colors[Log::Error] = Colours::Red;
		status_colors[Log::Critical] = Colours::Crimson;
		status_colors[Log::Alarm] = Colours::Firebrick;
		status_colors[Log::Panic] = Colours::Firebrick;
	}
}

void Statuslinelet::fill_extent(float x, float y, float* width, float* height) {
	SET_BOX(width, fmax(this->available_visible_width(x), 0.0F));
	
	if (this->lines == 0) {
		SET_BOX(height, fmax(this->available_visible_height(y), 0.0F));
	} else {
		SET_BOX(height, status_height * float(this->lines));
	}
}

void Statuslinelet::draw(CanvasDrawingSession^ ds, float x, float y, float Width, float Height) {
	size_t total = this->messages.size();
	
	ds->FillRectangle(x, y, Width, Height, Colours::Background);

	if (total > 0) {	
		this->section.lock_shared();
		
		auto mit = this->messages.begin();
		auto cit = this->colors.begin();

		if (this->lines == 0) {
			float flcount = Height / status_height;
			size_t alines = (size_t)(std::ceilf(flcount));

			if (total > alines) {
				for (size_t i = alines; i < total; i++) {
					mit++;
					cit++;
				}

				total = alines;
				y -= (status_height * (float(alines) - flcount));
			}
		}

		for (size_t idx = 0; idx < total; idx++, mit++, cit++) {
			float content_y = y + status_height * float(idx);
			
			content_y += std::fmaxf((status_height - (*mit)->LayoutBounds.Height) * 0.5F, 0.0F);
			ds->DrawTextLayout((*mit), x, content_y, (*cit));
		}

		this->section.unlock_shared();
	}
}

void Statuslinelet::append_message(Platform::String^ message, Log level) {
	auto lcolor = status_colors[level];

	this->section.lock();
	
	if (this->lines > 0) {
		if (this->messages.size() == this->lines) {
			this->colors.pop_front();
			this->messages.pop_front();
		}
	}

	this->colors.push_back((lcolor == nullptr) ? status_colors[Log::_] : lcolor);
	this->messages.push_back(make_text_layout(message, status_font));
	this->notify_updated();
	
	this->section.unlock();
}

void Statuslinelet::on_log_message(Log level, Platform::String^ message, SyslogMetainfo& data, Platform::String^ topic) {
	this->append_message("[" + level.ToString() + "] " + string_first_line(message), level);
}
