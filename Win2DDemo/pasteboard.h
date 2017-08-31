#pragma once

#include "canvas.h"
#include "snip/snip.h"

namespace Win2D::UIElement {
    public ref class IPasteboard : public Win2DCanvas {
    public:
        virtual ~IPasteboard();

    internal:
        IPasteboard(Windows::UI::Xaml::Controls::Panel^ parent, Platform::String^ id);

    internal:
        void Draw(Microsoft::Graphics::Canvas::CanvasDrawingSession^ ds) override;
    
    internal:
        void Insert(Snip* snip, float x = 0.0, float y = 0.0);
        void MoveTo(Snip* snip, float x, float y);

    private protected:
        virtual void BeforeInsert(Snip* snip, float x, float y) {};
        virtual void AfterInsert(Snip* snip, float x, float y) {};

    private protected:
        Snip* headSnip;
        Snip* tailSnip;
    };

    public ref class Pasteboard sealed : public IPasteboard {
    public:
        Pasteboard(Windows::UI::Xaml::Controls::Panel^ parent, Platform::String^ id);

    internal:
        void ChangeSize(double width, double height) override;
    };

    public ref class VerticalPasteboard sealed : public IPasteboard {
    public:
        VerticalPasteboard(Windows::UI::Xaml::Controls::Panel^ parent, Platform::String^ id);
        VerticalPasteboard(Windows::UI::Xaml::Controls::Panel^ parent, Platform::String^ id, float gapsize);

    internal:
        void ChangeSize(double width, double height) override;

    private protected:
        void BeforeInsert(Snip* snip, float x, float y) override;
        void AfterInsert(Snip* snip, float x, float y) override;

    private:
        float gapsize;
        float lastPosition;
    };
}
