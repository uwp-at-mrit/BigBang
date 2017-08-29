#pragma once

#include "canvas.h"

namespace Win2D::UIElement {
    private enum SnipTypes {
        Text
    };

    private class Snip {
    public:
        virtual ~Snip() noexcept {};

    public:
        virtual SnipTypes GetType() = 0;
        virtual TextExtent GetExtent(Microsoft::Graphics::Canvas::CanvasDrawingSession^ ds, double x, double y) = 0;
        virtual void Draw(Microsoft::Graphics::Canvas::CanvasDrawingSession^ ds, double x, double y) = 0;

    public:
        void* assocData;

    public:
        Snip* next;
        Snip* prev;
    };

    /*********************************************************************************************/
    public ref class Pasteboard sealed : public Win2DCanvas {
    internal:
        Pasteboard(Windows::UI::Xaml::Controls::Panel^ parent, Platform::String^ id);

    internal:
        void Draw(Microsoft::Graphics::Canvas::CanvasDrawingSession^ ds) override;
    
    internal:
        void Insert(Snip* snip, double x = 0.0, double y = 0.0);
        void MoveTo(Snip* snip, double x, double y);

    private protected:
        virtual void BeforeInsert(Snip* snip, double x, double y) {};
        virtual void AfterInsert(Snip* snip, double x, double y) {};

    private:
        Snip* headSnip;

    private:
        ~Pasteboard();
    };
}
