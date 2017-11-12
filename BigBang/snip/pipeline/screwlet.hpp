#pragma once

#include "snip/pipeline/pipesnip.hpp"

namespace WarGrey::SCADA {
    private class Screwlet : public WarGrey::SCADA::IPipeSnip {
    public:
        Screwlet(float width, float height, float thickness, double color, double saturation, double light, double highlight);

    public:
        void load() override;
        void fill_extent(float x, float y, float* w = nullptr, float* h = nullptr) override;
        void draw(Microsoft::Graphics::Canvas::CanvasDrawingSession^ ds, float x, float y, float Width, float Height) override;

    protected:
        virtual Microsoft::Graphics::Canvas::Geometry::CanvasGeometry^ make_fitting(float rx, float ry) = 0;
        virtual void locate_body(float x, float base_width, float body_off, float *base_x, float* body_x) = 0;
        virtual float locate_pipe(float x, float body_x, float fitting_rx, float fitting_off, float *pipe_x, float* fitting_x) = 0;

    protected:
        float width;
        float height;
        float pipe_thickness;
        float fitting_width;

    protected:
        Windows::UI::Color color;
        Windows::UI::Color fitting_color;
        Windows::UI::Color highlight_color;
        Windows::UI::Color body_color;
        Windows::UI::Color base_color;
        Microsoft::Graphics::Canvas::Geometry::CanvasCachedGeometry^ base_fitting;
        Microsoft::Graphics::Canvas::Geometry::CanvasCachedGeometry^ outlet_fitting;
        Microsoft::Graphics::Canvas::Brushes::CanvasLinearGradientBrush^ basefit_brush;
        Microsoft::Graphics::Canvas::Brushes::CanvasLinearGradientBrush^ outfit_brush;
        Microsoft::Graphics::Canvas::Brushes::CanvasLinearGradientBrush^ pipe_brush;
    };

    private class LScrewlet : public WarGrey::SCADA::Screwlet {
    public:
        LScrewlet(float width, float height, float thickness = 0.0F,
            double color = 120.0, double saturation = 0.607,
            double light = 0.339, double highlight = 0.839);
    
    public:
        Windows::Foundation::Rect get_input_port() override;
        Windows::Foundation::Rect get_output_port() override;

    protected:
        Microsoft::Graphics::Canvas::Geometry::CanvasGeometry^ make_fitting(float rx, float ry) override;
        void locate_body(float x, float base_width, float body_off, float *base_x, float* body_x) override;
        float locate_pipe(float x, float body_x, float fitting_rx, float fitting_off, float *pipe_x, float* fitting_x) override;
    };

    private class RScrewlet : public WarGrey::SCADA::Screwlet {
    public:
        RScrewlet(float width, float height, float thickness = 0.0F,
            double color = 120.0, double saturation = 0.607,
            double light = 0.339, double highlight = 0.839);

    public:
        Windows::Foundation::Rect get_input_port() override;
        Windows::Foundation::Rect get_output_port() override;

    protected:
        Microsoft::Graphics::Canvas::Geometry::CanvasGeometry^ make_fitting(float rx, float ry) override;
        void locate_body(float x, float base_width, float body_off, float *base_x, float* body_x) override;
        float locate_pipe(float x, float body_x, float fitting_rx, float fitting_off, float *pipe_x, float* fitting_x) override;
    };
}
