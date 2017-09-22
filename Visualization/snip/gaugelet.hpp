#pragma once

#include "snip/snip.hpp"

namespace WarGrey::SCADA {
    private class Gaugelet : public WarGrey::SCADA::Snip {
    public:
        Gaugelet(Platform::String^ caption, float ampere , int rpm);

    public:
        void draw(Microsoft::Graphics::Canvas::CanvasDrawingSession^ ds, float x, float y, float Width, float Height) override;
        void fill_extent(float x, float y, float* w = nullptr, float* h = nullptr,
            float* d = nullptr, float* s = nullptr, float* l = nullptr, float* r = nullptr)
            override;

    private:
        Microsoft::Graphics::Canvas::Text::CanvasTextFormat^ label_font;
        Microsoft::Graphics::Canvas::Text::CanvasTextFormat^ scale_font;
        Platform::String^ caption;
        float ampere;
        int rpm;
    };
}
