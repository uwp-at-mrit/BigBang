#pragma once

#include "graphlet/primitive.hpp"

namespace WarGrey::SCADA {
    private class Motorlet : public virtual WarGrey::SCADA::IGraphlet {
    public:
        Motorlet(float width, float height = 0.0F);

    public:
        void construct() override;
		void fill_extent(float x, float y, float* w = nullptr, float* h = nullptr) override;
		void update(long long count, long long interval, long long uptime) override;
        void draw(Microsoft::Graphics::Canvas::CanvasDrawingSession^ ds, float x, float y, float Width, float Height) override;
        
    private:
        float width;
        float height;

    private:
        Microsoft::Graphics::Canvas::Brushes::CanvasLinearGradientBrush^ serew_brush;
        Microsoft::Graphics::Canvas::Brushes::CanvasLinearGradientBrush^ head_brush;
        Microsoft::Graphics::Canvas::Brushes::CanvasLinearGradientBrush^ body_brush;
        Microsoft::Graphics::Canvas::Geometry::CanvasCachedGeometry^ head;
        Microsoft::Graphics::Canvas::Geometry::CanvasCachedGeometry^ body;
        Microsoft::Graphics::Canvas::Geometry::CanvasCachedGeometry^ lines;
        Microsoft::Graphics::Canvas::Geometry::CanvasCachedGeometry^ outline;
        Microsoft::Graphics::Canvas::Geometry::CanvasCachedGeometry^ parts;
        Microsoft::Graphics::Canvas::Geometry::CanvasCachedGeometry^ status;
    };
}
