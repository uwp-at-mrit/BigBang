#pragma once

#include "snip/snip.hpp"

namespace WarGrey::SCADA {
    private class Vibratorlet : public WarGrey::SCADA::Snip {
    public:
        Vibratorlet(float width);
        Vibratorlet(float width, float height);

    public:
        void draw(Microsoft::Graphics::Canvas::CanvasDrawingSession^ ds, float x, float y, float Width, float Height) override;
        void fill_extent(float x, float y, float* w = nullptr, float* h = nullptr,
            float* d = nullptr, float* s = nullptr, float* l = nullptr, float* r = nullptr)
            override;

    private:
        float width;
        float height;
    };
}
