#pragma once

#include "snip/snip.hpp"

namespace WarGrey::SCADA {
    private class Gaugelet : public WarGrey::SCADA::Snip {
    public:
        Gaugelet(Platform::String^ caption, int ampere , int rpm, char step = 10);

    public:
        void draw(Microsoft::Graphics::Canvas::CanvasDrawingSession^ ds, float x, float y, float Width, float Height) override;
        void fill_extent(float x, float y, float* w = nullptr, float* h = nullptr,
            float* d = nullptr, float* s = nullptr, float* l = nullptr, float* r = nullptr)
            override;
    
    private:
        void draw_gauge(Microsoft::Graphics::Canvas::CanvasDrawingSession^ ds,
            float x, float y, int mscale, float scale, Platform::String^ label,
            Windows::UI::Color& color);

    private:
        Microsoft::Graphics::Canvas::Text::CanvasTextFormat^ label_font;
        Microsoft::Graphics::Canvas::Text::CanvasTextFormat^ scale_font;
        Platform::String^ caption;
        unsigned int Ampere;
        unsigned int RPM;
        char step;

    private:
        float width;
        float height;
        float caption_width;
        float label_height;
        float scale_height;
        float gauge_width;
        float gauge_gapsize;
        float mark_width;
        float mark_interval;
        float gauge_y;

    private:
        float ampere;
        int rpm;
    };
}
