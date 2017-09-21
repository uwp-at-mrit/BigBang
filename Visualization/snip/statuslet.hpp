#pragma once

#include "snip.hpp"

namespace WarGrey::SCADA {
    ref class StatusTimer;

    private class Statuslet : public WarGrey::SCADA::Snip {
    public:
        Statuslet(Platform::String^ caption);
        Windows::Foundation::TimeSpan update_timestamp();

    public:
        void draw(Microsoft::Graphics::Canvas::CanvasDrawingSession^ ds, float x, float y, float Width, float Height) override;
        void fill_extent(float* w = nullptr, float* h = nullptr,
            float* d = nullptr, float* s = nullptr, float* l = nullptr, float* r = nullptr)
            override;

    private:
        Platform::String^ caption;
        Platform::String^ timestamp;
        Microsoft::Graphics::Canvas::Text::CanvasTextFormat^ layout_config;

    private:
        WarGrey::SCADA::StatusTimer^ timer;
    };
}
