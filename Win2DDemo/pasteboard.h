#pragma once

#include "canvas.h"

namespace Win2D::UIElement {
    private enum SnipTypes {
        Text
    };

    private class Snip {
    public:
        virtual SnipTypes GetType() = 0;
        virtual ~Snip() noexcept {};

    public:
        virtual void FillExtent(
            Microsoft::Graphics::Canvas::CanvasDrawingSession^ ds,
            float x, float y, float* width =nullptr, float* height = nullptr,
            float* descent = nullptr, float* space = nullptr, float* lspace = nullptr, float* rspace = nullptr)
            = 0;

        virtual void Draw(Microsoft::Graphics::Canvas::CanvasDrawingSession^ ds, float x, float y) = 0;

    public:
        void* assocData;

    public:
        Snip* next;
        Snip* prev;
    };

    /*********************************************************************************************/
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
    };

    public ref class VerticalPasteboard sealed : public IPasteboard {
    public:
        VerticalPasteboard(Windows::UI::Xaml::Controls::Panel^ parent, Platform::String^ id, int gapsize);

    private protected:
        void BeforeInsert(Snip* snip, float x, float y) override;
        void AfterInsert(Snip* snip, float x, float y) override;

    private:
        int gapsize;
        double lastPosition;
    };
}
