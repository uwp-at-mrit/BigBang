#pragma once

namespace Win2D::Sniplet {
    enum SnipTypes {
        Text
    };

    class Snip {
    public:
        virtual ~Snip() = 0;

    public:
        virtual SnipTypes GetType() = 0;
        virtual Windows::Foundation::Size GetSize(Microsoft::Graphics::Canvas::CanvasDrawingSession^ ds) = 0;
        virtual void Draw(Microsoft::Graphics::Canvas::CanvasDrawingSession^ ds) = 0;

    public:
        Snip* next;
        Snip* prev;
    };
}
