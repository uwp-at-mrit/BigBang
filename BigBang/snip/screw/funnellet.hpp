#pragma once

#include <collection.h>
#include <algorithm>

#include "snip/screw/screwsnip.hpp"

namespace WarGrey::SCADA {
    private class Funnellet : public WarGrey::SCADA::IMotorSnip {
    public:
        Funnellet(float width, float height = 0.0F,
            double color = 120.0, double saturation = 1.0,
            double dark_lightness = 0.4, double light_lightness = 0.8);

    public:
        void load() override;
        void update(long long count, long long interval, long long uptime, bool is_slow) override;
        void draw(Microsoft::Graphics::Canvas::CanvasDrawingSession^ ds, float x, float y, float Width, float Height) override;
        void fill_extent(float x, float y, float* w = nullptr, float* h = nullptr) override;

    public:
        Windows::Foundation::Rect get_input_port() override;
        Windows::Foundation::Rect get_output_port() override;
		Windows::Foundation::Rect get_motor_port() override;

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
