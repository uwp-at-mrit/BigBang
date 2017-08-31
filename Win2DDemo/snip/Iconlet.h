#pragma once

#include "snip/snip.h"

namespace Win2D::Snip {
    private class Iconlet : public Win2D::UIElement::Snip {
    public:
        ~Iconlet() noexcept;
        Iconlet(float size);
        Win2D::UIElement::SnipTypes GetType() override;

    public:
        void FillExtent(float x, float y,
            float* w = nullptr, float* h = nullptr,
            float* d = nullptr, float* s = nullptr,
            float* l = nullptr, float* r = nullptr)
            override;

        void Draw(Microsoft::Graphics::Canvas::CanvasDrawingSession^ ds, float x, float y) override;

    private:
        float size;
    };
}
