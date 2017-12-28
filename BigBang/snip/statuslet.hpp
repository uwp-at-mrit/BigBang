#pragma once

#include "snip.hpp"
#include "modbus.hpp"

namespace WarGrey::SCADA {
    private class Statuslet : public WarGrey::SCADA::Snip {
    public:
        Statuslet(Platform::String^ caption, Platform::String^ plc, IModbusConfirmation* callback);

    public:
        void load() override;
        void update(long long count, long long interval, long long uptime, bool is_slow) override;
        void draw(Microsoft::Graphics::Canvas::CanvasDrawingSession^ ds, float x, float y, float Width, float Height) override;
        void fill_extent(float x, float y, float* w = nullptr, float* h = nullptr) override;

    private:
        WarGrey::SCADA::IModbusClient* client;
		Platform::String^ caption;

	private:
		Microsoft::Graphics::Canvas::Text::CanvasTextFormat^ label_font;
    };
}
