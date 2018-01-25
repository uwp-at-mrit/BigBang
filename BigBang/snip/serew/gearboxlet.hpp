#pragma once

#include "snip/serew/serewsnip.hpp"

namespace WarGrey::SCADA {
    private class Gearboxlet : public WarGrey::SCADA::IMotorSnip {
    public:
        Gearboxlet(float width, float height, float thickness, double color, double saturation, double light, double highlight);

    public:
        void construct() override;
        void fill_extent(float x, float y, float* w = nullptr, float* h = nullptr) override;
        void draw(Microsoft::Graphics::Canvas::CanvasDrawingSession^ ds, float x, float y, float Width, float Height) override;

    protected:
        virtual Microsoft::Graphics::Canvas::Geometry::CanvasGeometry^ make_fitting(float rx, float ry) = 0;
        virtual void locate_body(float x, float base_width, float body_off, float* body_x, float *base_x) = 0;
        virtual void locate_pipe(float x, float body_x, float offrate, float basefit_rx, float outfit_rx,
            float* pipe_x, float* basefit_x, float *basefit_cx, float* outfit_x, float* outfit_cx) = 0;

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
        Windows::UI::Color gbox_color;
        Microsoft::Graphics::Canvas::Geometry::CanvasCachedGeometry^ base_fitting;
        Microsoft::Graphics::Canvas::Geometry::CanvasCachedGeometry^ outlet_fitting;
        Microsoft::Graphics::Canvas::Brushes::CanvasLinearGradientBrush^ basefit_brush;
        Microsoft::Graphics::Canvas::Brushes::CanvasLinearGradientBrush^ outfit_brush;
        Microsoft::Graphics::Canvas::Brushes::CanvasLinearGradientBrush^ pipe_brush;
    };

    private class LGearboxlet : public WarGrey::SCADA::Gearboxlet {
    public:
        LGearboxlet(float width, float height, float thickness = 0.0F,
            double color = 120.0, double saturation = 0.607,
            double light = 0.339, double highlight = 0.839);
    
    public:
        Windows::Foundation::Rect get_input_port() override;
        Windows::Foundation::Rect get_output_port() override;
		Windows::Foundation::Rect get_motor_port() override;

    protected:
        Microsoft::Graphics::Canvas::Geometry::CanvasGeometry^ make_fitting(float rx, float ry) override;
        void locate_body(float x, float base_width, float body_off, float *body_x, float *base_x) override;
        void locate_pipe(float x, float body_x, float offrate, float basefit_rx, float outfit_rx,
            float* pipe_x, float* basefit_x, float *basefit_cx, float* outfit_x, float* outfit_cx) override;
    };

    private class RGearboxlet : public WarGrey::SCADA::Gearboxlet {
    public:
        RGearboxlet(float width, float height, float thickness = 0.0F,
            double color = 120.0, double saturation = 0.607,
            double light = 0.339, double highlight = 0.839);

    public:
        Windows::Foundation::Rect get_input_port() override;
        Windows::Foundation::Rect get_output_port() override;
		Windows::Foundation::Rect get_motor_port() override;

    protected:
        Microsoft::Graphics::Canvas::Geometry::CanvasGeometry^ make_fitting(float rx, float ry) override;
        void locate_body(float x, float base_width, float body_off, float* body_x, float *base_x) override;
        void locate_pipe(float x, float body_x, float offrate, float basefit_rx, float outfit_rx,
            float* pipe_x, float* basefit_x, float *basefit_cx, float* outfit_x, float* outfit_cx) override;
    };
}
