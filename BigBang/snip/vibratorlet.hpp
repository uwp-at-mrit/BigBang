#pragma once

#include "snip/snip.hpp"

namespace WarGrey::SCADA {
    private class Vibratorlet : public WarGrey::SCADA::Snip {
    public:
        Vibratorlet(float width, float height = 0.0F);

    public:
        void load() override;
        void update(long long count, long long interval, long long uptime, bool is_slow) override;
        void draw(Microsoft::Graphics::Canvas::CanvasDrawingSession^ ds, float x, float y, float Width, float Height) override;
        void fill_extent(float x, float y, float* w = nullptr, float* h = nullptr) override;

    private:
        void initialize_hat();
        Microsoft::Graphics::Canvas::Geometry::CanvasCachedGeometry^ hat_decorator_sides;
        Microsoft::Graphics::Canvas::Geometry::CanvasCachedGeometry^ hat_frontend_midbg;
        Microsoft::Graphics::Canvas::Geometry::CanvasCachedGeometry^ hat_frontend_midfg;
        Microsoft::Graphics::Canvas::Geometry::CanvasCachedGeometry^ hat_bottom;
        Microsoft::Graphics::Canvas::Geometry::CanvasGeometry^ hat;
        Microsoft::Graphics::Canvas::Brushes::ICanvasBrush^ hat_brush;
        float hat_adjust_yoff;

    private:
        void initialize_rings();
        Microsoft::Graphics::Canvas::Geometry::CanvasCachedGeometry^ rings;
        Microsoft::Graphics::Canvas::Geometry::CanvasGeometry^ body;
        Microsoft::Graphics::Canvas::Brushes::ICanvasBrush^ ring_brush;
        float ring_interval;

    private:
        float width;
        float height;

    private:
        bool vibrated;
    };
}
