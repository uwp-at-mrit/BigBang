#pragma once

#include <collection.h>
#include <algorithm>

#include "snip/snip.hpp"

namespace WarGrey::SCADA {
    private class Funnellet : public WarGrey::SCADA::Snip {
    public:
        Funnellet(float width, float height = 0.0F,
            double color = 120.0, double saturation = 1.0,
            double dark_lightness = 4.0, double light_lightness = 0.8);

    public:
        void load() override;
        void update(long long count, long long interval, long long uptime, bool is_slow) override;
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

        Platform::Collections::Vector<Microsoft::Graphics::Canvas::Geometry::CanvasCachedGeometry^>^ particles;
        float particles_width;

        Windows::UI::Color color;
        Windows::UI::Color highlight_color;
    };
}
