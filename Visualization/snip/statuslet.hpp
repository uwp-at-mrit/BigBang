#pragma once

#include "snip.hpp"

namespace WarGrey::SCADA {
    ref class StatusTimer;

    private class Statuslet : public WarGrey::SCADA::Snip {
    public:
        Statuslet(Platform::String^ caption);
        void on_attach_to(Pasteboard^ master);

    public:
        void draw(Microsoft::Graphics::Canvas::CanvasDrawingSession^ ds, float x, float y, float Width, float Height) override;
        void fill_extent(float x, float y, float* w = nullptr, float* h = nullptr,
            float* d = nullptr, float* s = nullptr, float* l = nullptr, float* r = nullptr)
            override;

    private:
        Windows::Foundation::TimeSpan update_timestamp();
        friend Windows::Foundation::TimeSpan update_surpass_ref_class(Statuslet* self);

    private:
        Platform::String^ caption;
        Platform::String^ timestamp;
        Microsoft::Graphics::Canvas::Text::CanvasTextFormat^ layout_config;

    private:
        WarGrey::SCADA::StatusTimer^ timer;
        WarGrey::SCADA::Pasteboard^ master;
    };
}
