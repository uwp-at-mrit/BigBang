#pragma once

#include "snip/pipeline/pipesnip.hpp"

namespace WarGrey::SCADA {
    private class Sleevelet : public WarGrey::SCADA::IPipeSnip {
    public:
        Sleevelet(float width, float height, float thickness, double color, double saturation, double light, double highlight);

    public:
        void load() override;
        void draw(Microsoft::Graphics::Canvas::CanvasDrawingSession^ ds, float x, float y, float Width, float Height) override;
        void fill_extent(float x, float y, float* w = nullptr, float* h = nullptr) override;

    protected:
        virtual Microsoft::Graphics::Canvas::Geometry::CanvasGeometry^ make_fitting(float rx, float ry) = 0;
        virtual void locate_pipe(float x, float rx, float off, float* infit_x, float *infit_cx, float* outfit_x, float* outfit_cx) = 0;

    protected:
        float width;
        float height;
        float thickness;
        float fitting_width;

    protected:
        Windows::UI::Color color;
        Windows::UI::Color highlight_color;
        Windows::UI::Color fitting_color;
        Microsoft::Graphics::Canvas::Geometry::CanvasCachedGeometry^ fitting;
        Microsoft::Graphics::Canvas::Geometry::CanvasCachedGeometry^ hollow_body;
        Microsoft::Graphics::Canvas::Brushes::CanvasLinearGradientBrush^ fitting_brush;
        Microsoft::Graphics::Canvas::Brushes::CanvasLinearGradientBrush^ brush;
        Microsoft::Graphics::Canvas::Geometry::CanvasGeometry^ body_mask;
        Microsoft::Graphics::Canvas::Geometry::CanvasStrokeStyle^ cartoon_style;
    };

    private class LSleevelet : public WarGrey::SCADA::Sleevelet {
    public:
        LSleevelet(float width, float height = 0.0F, float thickness = 0.0F,
            double color = nan("Silver"), double saturation = 0.0,
            double light = 0.512, double highlight = 0.753);

    public:
        void update(long long count, long long interval, long long uptime, bool is_slow) override;
        
    public:
        Windows::Foundation::Rect get_input_port() override;
        Windows::Foundation::Rect get_output_port() override;

    protected:
        Microsoft::Graphics::Canvas::Geometry::CanvasGeometry^ make_fitting(float rx, float ry) override;
        void locate_pipe(float x, float rx, float off, float* infit_x, float *infit_cx, float* outfit_x, float* outfit_cx) override;
    };

    private class RSleevelet : public WarGrey::SCADA::Sleevelet {
    public:
        RSleevelet(float width, float height = 0.0F, float thickness = 0.0F,
            double color = nan("Silver"), double saturation = 0.0,
            double light = 0.512, double highlight = 0.753);

    public:
        void update(long long count, long long interval, long long uptime, bool is_slow) override;

    public:
        Windows::Foundation::Rect get_input_port() override;
        Windows::Foundation::Rect get_output_port() override;

    protected:
        Microsoft::Graphics::Canvas::Geometry::CanvasGeometry^ make_fitting(float rx, float ry) override;
        void locate_pipe(float x, float rx, float off, float* infit_x, float *infit_cx, float* outfit_x, float* outfit_cx) override;
    };
}
