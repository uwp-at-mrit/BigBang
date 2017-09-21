#pragma once

#include "snip.hpp"

namespace WarGrey::SCADA {
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
        Microsoft::Graphics::Canvas::Text::CanvasTextFormat^ font;
        Platform::String^ caption;
        bool plc_connected;
    };
}
