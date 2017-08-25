#pragma once

namespace Win2D::UIElement {
    class Snip {
    public:
        Snip();
        ~Snip();

    public:
        virtual void Draw(Microsoft::Graphics::Canvas::CanvasDrawingSession^ ds) = 0;

    public:
        Snip* next;
        Snip* prev;
    };
}
