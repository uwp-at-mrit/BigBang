#pragma once

#include "snip.hpp"
#include "IPLCClient.hpp"

namespace WarGrey::SCADA {
	float statusbar_height();

    private class Statusbarlet : public WarGrey::SCADA::ISnip {
    public:
		Statusbarlet(Platform::String^ caption, WarGrey::SCADA::IPLCClient* device = nullptr);

    public:
        void construct() override;
		void fill_extent(float x, float y, float* w = nullptr, float* h = nullptr) override;
        void update(long long count, long long interval, long long uptime) override;
        void draw(Microsoft::Graphics::Canvas::CanvasDrawingSession^ ds, float x, float y, float Width, float Height) override;
        
    private:
		Microsoft::Graphics::Canvas::Text::CanvasTextLayout^ caption;
		Microsoft::Graphics::Canvas::Text::CanvasTextLayout^ device_name;
		WarGrey::SCADA::IPLCClient* device;
    };

	private class Statuslinelet : public WarGrey::SCADA::ISnip, public WarGrey::SCADA::ISyslogReceiver {
	public:
		Statuslinelet(WarGrey::SCADA::Log level) : ISyslogReceiver(level) {}

	public:
		void construct() override;
		void draw(Microsoft::Graphics::Canvas::CanvasDrawingSession^ ds, float x, float y, float Width, float Height) override;
		void fill_extent(float x, float y, float* w = nullptr, float* h = nullptr) override;

	public:
		void set_message(Platform::String^ message, WarGrey::SCADA::Log level = WarGrey::SCADA::Log::None);
		void on_log_message(WarGrey::SCADA::Log level, Platform::String^ message,
			WarGrey::SCADA::SyslogMetainfo& data, Platform::String^ topic) override;

	private:
		Microsoft::Graphics::Canvas::Brushes::ICanvasBrush^ color;
		Microsoft::Graphics::Canvas::Text::CanvasTextLayout^ status;
	};
}
