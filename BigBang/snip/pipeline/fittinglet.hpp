#pragma once

#include "snip/pipeline/pipesnip.hpp"

namespace WarGrey::SCADA {
    private class Fittinglet : public WarGrey::SCADA::IPipeSnip {
    public:
        Fittinglet(float width, float height, float socket_height, double color, double saturation, double light, double highlight);

    public:
        void load() override;
        void fill_extent(float x, float y, float* w = nullptr, float* h = nullptr) override;

    protected:
        virtual Microsoft::Graphics::Canvas::Geometry::CanvasCachedGeometry^ make_body(float rx, float ry, float width) = 0;

    protected:
        float width;
        float height;
        float socket_height;

    protected:
        Windows::UI::Color color;
        Windows::UI::Color body_color;
        Windows::UI::Color highlight_color;
        Microsoft::Graphics::Canvas::Geometry::CanvasCachedGeometry^ body;
        Microsoft::Graphics::Canvas::Brushes::CanvasLinearGradientBrush^ brush;
    };

    private class LFittinglet : public WarGrey::SCADA::Fittinglet {
    public:
        LFittinglet(float width, float height = 0.0F, float socket_height = 0.0F,
            double color = nan("Silver"), double saturation = 0.0,
            double light = 0.512, double highlight = 0.753);

    public:
        void draw(Microsoft::Graphics::Canvas::CanvasDrawingSession^ ds, float x, float y, float Width, float Height) override;
        
    public:
        Windows::Foundation::Rect get_inlet() override;
        Windows::Foundation::Rect get_outlet() override;

    protected:
        Microsoft::Graphics::Canvas::Geometry::CanvasCachedGeometry^ make_body(float rx, float ry, float width) override;
    };

    private class RFittinglet : public WarGrey::SCADA::Fittinglet {
    public:
        RFittinglet(float width, float height = 0.0F, float socket_height = 0.0F,
            double color = 120.0, double saturation = 0.607,
            double light = 0.339, double highlight = 0.839);

    public:
        void draw(Microsoft::Graphics::Canvas::CanvasDrawingSession^ ds, float x, float y, float Width, float Height) override;

    public:
        Windows::Foundation::Rect get_inlet() override;
        Windows::Foundation::Rect get_outlet() override;

    protected:
        Microsoft::Graphics::Canvas::Geometry::CanvasCachedGeometry^ make_body(float rx, float ry, float width) override;
    };
}
