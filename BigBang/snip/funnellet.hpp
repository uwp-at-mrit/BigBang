#pragma once

#include "snip/snip.hpp"

namespace WarGrey::SCADA {
    private class Funnellet : public WarGrey::SCADA::Snip {
    public:
        Funnellet(float width, float height = 0.0F,
            double color = 120.0, double saturation = 1.0,
            double dark_lightness = 4.0, double light_lightness = 0.8);

    public:
        void load() override;
        void draw(Microsoft::Graphics::Canvas::CanvasDrawingSession^ ds, float x, float y, float Width, float Height) override;
        void fill_extent(float x, float y, float* w = nullptr, float* h = nullptr,
            float* d = nullptr, float* s = nullptr, float* l = nullptr, float* r = nullptr)
            override;

    private:
        float width;
        float height;

    private:
        Microsoft::Graphics::Canvas::Geometry::CanvasCachedGeometry^ topface;
        Microsoft::Graphics::Canvas::Geometry::CanvasCachedGeometry^ body;

        Microsoft::Graphics::Canvas::Brushes::CanvasLinearGradientBrush^ topface_brush;
        Microsoft::Graphics::Canvas::Brushes::CanvasLinearGradientBrush^ body_brush;

        Windows::UI::Color color;
        Windows::UI::Color highlight_color;
    };
}
