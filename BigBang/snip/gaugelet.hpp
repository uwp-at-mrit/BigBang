#pragma once

#include "snip/snip.hpp"

namespace WarGrey::SCADA {
    private class Gaugelet : public WarGrey::SCADA::Snip {
    public:
        Gaugelet(Platform::String^ caption, int ampere , int rpm, unsigned char step = 10,
            Windows::UI::Color ampere_color = Windows::UI::Colors::RoyalBlue,
            Windows::UI::Color rpm_color = Windows::UI::Colors::Green);

    public:
        void load() override;
        void draw(Microsoft::Graphics::Canvas::CanvasDrawingSession^ ds, float x, float y, float Width, float Height) override;
        void fill_extent(float x, float y, float* w = nullptr, float* h = nullptr,
            float* d = nullptr, float* s = nullptr, float* l = nullptr, float* r = nullptr)
            override;
    
    private:
        void initialize_meters();
        void draw_meter(Microsoft::Graphics::Canvas::CanvasDrawingSession^ ds,
            float x, float y,
            Microsoft::Graphics::Canvas::Text::CanvasTextLayout^ scales,
            int mscale, float scale, Platform::String^ label,
            Windows::UI::Color& color);

    private:
        Microsoft::Graphics::Canvas::Text::CanvasTextFormat^ label_font;
        Microsoft::Graphics::Canvas::Text::CanvasTextLayout^ ampere_scales;
        Microsoft::Graphics::Canvas::Text::CanvasTextLayout^ rpm_scales;
        Microsoft::Graphics::Canvas::Geometry::CanvasCachedGeometry^ scale_marks;
        Platform::String^ caption;
        Windows::UI::Color Acolor;
        Windows::UI::Color Rcolor;
        unsigned int Ampere;
        unsigned int RPM;
        unsigned char step;

    private:
        float width;
        float height;
        float caption_width;
        float label_height;
        float meter_width;
        float meter_gapsize;
        float mark_width;
        float mark_interval;

    private:
        float ampere;
        int rpm;
    };
}
