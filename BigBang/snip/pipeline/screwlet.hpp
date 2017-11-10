#pragma once

#include "snip/pipeline/pipesnip.hpp"

namespace WarGrey::SCADA {
    private class Screwlet : public WarGrey::SCADA::IPipeSnip {
    public:
        Screwlet(float width, float height, float thickness = 0.0F,
            double color = 120.0, double saturation = 0.607,
            double light = 0.339, double highlight = 0.839);

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
        float pipe_thickness;
        float fitting_width;

    private:
        Windows::UI::Color color;
        Windows::UI::Color fitting_color;
        Windows::UI::Color highlight_color;
        Windows::UI::Color body_color;
        Windows::UI::Color base_color;
        Microsoft::Graphics::Canvas::Geometry::CanvasCachedGeometry^ fitting;
        Microsoft::Graphics::Canvas::Brushes::CanvasLinearGradientBrush^ fitting_brush;
        Microsoft::Graphics::Canvas::Brushes::CanvasLinearGradientBrush^ pipe_brush;
    };
}
