#pragma once

#include <cmath>
#include "snip/snip.hpp"

namespace WarGrey::SCADA {
    private class IPipelet : public WarGrey::SCADA::Snip {
    public:
        virtual void fill_inport_extent(float* x, float* y, float* width = nullptr, float* height = nullptr) = 0;
        virtual void fill_outport_extent(float* x, float* y, float* width = nullptr, float* height = nullptr) = 0;
    };

    private class Screwlet : public WarGrey::SCADA::IPipelet {
    public:
        Screwlet(float width, float height, float thickness = 0.0F,
            double color = 120.0, double saturation = 0.607,
            double bodylight = 0.339, double highlight = 0.839);

    public:
        void load() override;
        void draw(Microsoft::Graphics::Canvas::CanvasDrawingSession^ ds, float x, float y, float Width, float Height) override;
        void fill_extent(float x, float y, float* w = nullptr, float* h = nullptr,
            float* d = nullptr, float* s = nullptr, float* l = nullptr, float* r = nullptr)
            override;

    public:
        void fill_inport_extent(float* x, float* y, float* width = nullptr, float* height = nullptr) override;
        void fill_outport_extent(float* x, float* y, float* width = nullptr, float* height = nullptr) override;

    private:
        float width;
        float height;
        float pipe_thickness;
        float connector_width;

    private:
        Windows::UI::Color color;
        Windows::UI::Color base_color;
        Windows::UI::Color connector_color;
        Windows::UI::Color highlight_color;
        Microsoft::Graphics::Canvas::Geometry::CanvasCachedGeometry^ connector;
        Microsoft::Graphics::Canvas::Brushes::CanvasLinearGradientBrush^ connector_brush;
        Microsoft::Graphics::Canvas::Brushes::CanvasLinearGradientBrush^ pipe_brush;
    };

    private class Pipelet : public WarGrey::SCADA::IPipelet {
    public:
        Pipelet(float width, float height = 0.0F, float thickness = 0.0F,
            double color = nan("DimGray"), double saturation = 0.0,
            double bodylight = 0.412, double highlight = 0.753);

    public:
        void load() override;
        void update(long long count, long long interval, long long uptime, bool is_slow) override;
        void draw(Microsoft::Graphics::Canvas::CanvasDrawingSession^ ds, float x, float y, float Width, float Height) override;
        void fill_extent(float x, float y, float* w = nullptr, float* h = nullptr,
            float* d = nullptr, float* s = nullptr, float* l = nullptr, float* r = nullptr)
            override;

    public:
        void fill_inport_extent(float* x, float* y, float* width = nullptr, float* height = nullptr) override;
        void fill_outport_extent(float* x, float* y, float* width = nullptr, float* height = nullptr) override;

    private:
        float width;
        float height;
        float thickness;
        float connector_width;

    private:
        Windows::UI::Color color;
        Windows::UI::Color connector_color;
        Windows::UI::Color highlight_color;
        Microsoft::Graphics::Canvas::Geometry::CanvasCachedGeometry^ connector;
        Microsoft::Graphics::Canvas::Geometry::CanvasCachedGeometry^ hollow_body;
        Microsoft::Graphics::Canvas::Geometry::CanvasGeometry^ body_mask;
        Microsoft::Graphics::Canvas::Geometry::CanvasStrokeStyle^ cartoon_style;
        Microsoft::Graphics::Canvas::Brushes::CanvasLinearGradientBrush^ connector_brush;
        Microsoft::Graphics::Canvas::Brushes::CanvasLinearGradientBrush^ brush;
    };

    private class GlueCleanerlet : public WarGrey::SCADA::IPipelet {
    public:
        GlueCleanerlet(float width, float height, float thickness = 0.0F,
            double color = 120.0, double saturation = 0.607,
            double light = 0.339, double highlight = 0.839);

    public:
        void load() override;
        void draw(Microsoft::Graphics::Canvas::CanvasDrawingSession^ ds, float x, float y, float Width, float Height) override;
        void fill_extent(float x, float y, float* w = nullptr, float* h = nullptr,
            float* d = nullptr, float* s = nullptr, float* l = nullptr, float* r = nullptr)
            override;

    public:
        void fill_inport_extent(float* x, float* y, float* width = nullptr, float* height = nullptr) override;
        void fill_outport_extent(float* x, float* y, float* width = nullptr, float* height = nullptr) override;

    private:
        float width;
        float height;
        float pipe_thickness;

    private:
        Windows::UI::Color color;
        Windows::UI::Color highlight_color;
        Windows::UI::Color body_color;
        Windows::UI::Color endpoint_color;
        Microsoft::Graphics::Canvas::Brushes::CanvasLinearGradientBrush^ hat_brush;
        Microsoft::Graphics::Canvas::Brushes::CanvasLinearGradientBrush^ hatbody_brush;
        Microsoft::Graphics::Canvas::Brushes::CanvasLinearGradientBrush^ pipe_brush;
        Microsoft::Graphics::Canvas::Geometry::CanvasCachedGeometry^ pipe;
        Microsoft::Graphics::Canvas::Geometry::CanvasCachedGeometry^ endpoint;
    };
}
