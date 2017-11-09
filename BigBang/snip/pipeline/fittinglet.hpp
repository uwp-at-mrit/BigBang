#pragma once

#include "snip/pipeline/pipesnip.hpp"

namespace WarGrey::SCADA {
    private class Fittinglet : public WarGrey::SCADA::IPipeSnip {
    public:
        Fittinglet(float width, float height = 0.0F, float thickness = 0.0F,
            double color = nan("Silver"), double saturation = 0.0,
            double light = 0.512, double highlight = 0.753);

    public:
        void load() override;
        void draw(Microsoft::Graphics::Canvas::CanvasDrawingSession^ ds, float x, float y, float Width, float Height) override;
        void fill_extent(float x, float y, float* w = nullptr, float* h = nullptr) override;

    public:
        Windows::Foundation::Rect get_inlet() override;
        Windows::Foundation::Rect get_outlet() override;

    private:
        float width;
        float height;
        float pipe_ascent;
        float pipe_thickness;

    private:
        Windows::UI::Color color;
        Windows::UI::Color body_color;
        Windows::UI::Color highlight_color;
        Microsoft::Graphics::Canvas::Geometry::CanvasCachedGeometry^ body;
        Microsoft::Graphics::Canvas::Brushes::CanvasLinearGradientBrush^ brush;
    };
}
