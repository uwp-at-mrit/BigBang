#pragma once

#include "pasteboard.h"

namespace Win2D::Sniplet {
    class Textlet : public Win2D::UIElement::Snip {
    public:
        ~Textlet() noexcept;
        Textlet(Platform::String^ content = "");

    public:
        Win2D::UIElement::SnipTypes GetType() override;
        Win2D::UIElement::TextExtent GetExtent(Microsoft::Graphics::Canvas::CanvasDrawingSession^ ds, double x, double y) override;
        void Draw(Microsoft::Graphics::Canvas::CanvasDrawingSession^ ds, double x, double y) override;

    private:
        Platform::String^ _content;
        Microsoft::Graphics::Canvas::Text::CanvasTextFormat^ _font;
    };
}
