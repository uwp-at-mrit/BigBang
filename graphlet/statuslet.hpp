#pragma once

#include <shared_mutex>
#include <list>

#include "graphlet/primitive.hpp"
#include "network/tcp.hpp"

namespace WarGrey::SCADA {
	float statusbar_height();

    private class Statusbarlet : public virtual WarGrey::SCADA::IGraphlet {
    public:
		Statusbarlet(WarGrey::SCADA::ITCPConnection* device = nullptr);

    public:
        void construct() override;
		void fill_extent(float x, float y, float* w = nullptr, float* h = nullptr) override;
        void update(long long count, long long interval, long long uptime) override;
        void draw(Microsoft::Graphics::Canvas::CanvasDrawingSession^ ds, float x, float y, float Width, float Height) override;

	public:
		void set_caption(Platform::String^ caption, bool force = false);
        
    private:
		Microsoft::Graphics::Canvas::Text::CanvasTextLayout^ caption;
		Microsoft::Graphics::Canvas::Text::CanvasTextLayout^ device_name;
		WarGrey::SCADA::ITCPConnection* device;
		WarGrey::SCADA::TCPMode plc_mode;

	private:
		Microsoft::Graphics::Canvas::Geometry::CanvasCachedGeometry^ retry_icon;
		Platform::String^ title;
		unsigned int retry_step;
		float retry_icon_size;
	};

	private class Statuslinelet : public virtual WarGrey::SCADA::IGraphlet, public WarGrey::SCADA::ISyslogReceiver {
	public:
		Statuslinelet(WarGrey::SCADA::Log level, unsigned int lines = 1);

	public:
		void construct() override;
		void draw(Microsoft::Graphics::Canvas::CanvasDrawingSession^ ds, float x, float y, float Width, float Height) override;
		void fill_extent(float x, float y, float* w = nullptr, float* h = nullptr) override;

	public:
		void push_message(Platform::String^ message, WarGrey::SCADA::Log level = WarGrey::SCADA::Log::_);
		void on_log_message(WarGrey::SCADA::Log level, Platform::String^ message,
			WarGrey::SCADA::SyslogMetainfo& data, Platform::String^ topic) override;

	private:
		std::list<Microsoft::Graphics::Canvas::Brushes::ICanvasBrush^> colors;
		std::list<Microsoft::Graphics::Canvas::Text::CanvasTextLayout^> messages;

	private:
		std::shared_mutex section;
		unsigned int lines;
	};
}
