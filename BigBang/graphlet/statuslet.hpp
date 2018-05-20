#pragma once

#include <shared_mutex>
#include <list>

#include "graphlet/primitive.hpp"
#include "IPLCMaster.hpp"

namespace WarGrey::SCADA {
	float statusbar_height();

    private class Statusbarlet : public virtual WarGrey::SCADA::IGraphlet {
    public:
		Statusbarlet(Platform::String^ caption, WarGrey::SCADA::IPLCMaster* device = nullptr);

    public:
        void construct() override;
		void fill_extent(float x, float y, float* w = nullptr, float* h = nullptr) override;
        void update(long long count, long long interval, long long uptime) override;
        void draw(Microsoft::Graphics::Canvas::CanvasDrawingSession^ ds, float x, float y, float Width, float Height) override;
        
    private:
		Microsoft::Graphics::Canvas::Text::CanvasTextLayout^ caption;
		Microsoft::Graphics::Canvas::Text::CanvasTextLayout^ device_name;
		WarGrey::SCADA::IPLCMaster* device;
    };

	private class Statuslinelet : public virtual WarGrey::SCADA::IGraphlet, public WarGrey::SCADA::ISyslogReceiver {
	public:
		Statuslinelet(WarGrey::SCADA::Log level, unsigned int lines = 1);

	public:
		void construct() override;
		void draw(Microsoft::Graphics::Canvas::CanvasDrawingSession^ ds, float x, float y, float Width, float Height) override;
		void fill_extent(float x, float y, float* w = nullptr, float* h = nullptr) override;

	public:
		void append_message(Platform::String^ message, WarGrey::SCADA::Log level = WarGrey::SCADA::Log::None);
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
