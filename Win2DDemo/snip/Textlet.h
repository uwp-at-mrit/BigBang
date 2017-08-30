#pragma once

#include "pasteboard.h"

namespace Win2D::Sniplet {
    class Textlet : public Win2D::UIElement::Snip {
    public:
        ~Textlet() noexcept;
        Textlet(Platform::String^ content = "");
        Win2D::UIElement::SnipTypes GetType() override;

    public:
        void FillExtent(float x, float y,
            float* w = nullptr, float* h = nullptr,
            float* d = nullptr, float* s = nullptr,
            float* l = nullptr, float* r = nullptr)
            override;

        void Draw(Microsoft::Graphics::Canvas::CanvasDrawingSession^ ds, float x, float y) override;

    private:
        Platform::String^ content;
        Microsoft::Graphics::Canvas::Text::CanvasTextFormat^ font;
    };
}
