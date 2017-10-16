#pragma once

#include "snip/snip.hpp"

namespace WarGrey::SCADA {
    private class Motorlet : public WarGrey::SCADA::Snip {
    public:
        Motorlet(float width);
        Motorlet(float width, float height);

    public:
        void load() override;
        void update(long long count, long long interval, long long uptime, bool is_slow) override;
        void draw(Microsoft::Graphics::Canvas::CanvasDrawingSession^ ds, float x, float y, float Width, float Height) override;
        void fill_extent(float x, float y, float* w = nullptr, float* h = nullptr,
            float* d = nullptr, float* s = nullptr, float* l = nullptr, float* r = nullptr)
            override;

    private:
        float width;
        float height;

    private:
        Microsoft::Graphics::Canvas::Brushes::ICanvasBrush^ screw_brush;
    };
}
