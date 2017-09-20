#pragma once

#include "snip.hpp"

namespace WarGrey::SCADA {
    private class Alarmlet : public WarGrey::SCADA::Snip {
    public:
        Alarmlet(float size);
        ~Alarmlet() noexcept;

    public:
        void change_text(Platform::String^ content);

    public:
        void fill_extent(float* w = nullptr, float* h = nullptr,
            float* d = nullptr, float* s = nullptr, float* l = nullptr, float* r = nullptr)
            override;

        void draw(Microsoft::Graphics::Canvas::CanvasDrawingSession^ ds, float x, float y, float Width, float Height) override;

    private:
        float size;
    };
}
