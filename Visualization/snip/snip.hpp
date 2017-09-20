#pragma once

#include "canvas.hxx"
#include "object.hpp"

namespace WarGrey::SCADA {
    private class Snip {
    public:
        virtual ~Snip() noexcept;

    public:
        virtual void fill_extent(float* width = nullptr, float* height = nullptr,
            float* descent = nullptr, float* space = nullptr, float* lspace = nullptr, float* rspace = nullptr)
            = 0;

        virtual void draw(
            Microsoft::Graphics::Canvas::CanvasDrawingSession^ ds,
            float x, float y, float Width, float Height)
            = 0;

    public:
        AbstractObject* info;

    public:
        Snip* next;
        Snip* prev;
    };
}
