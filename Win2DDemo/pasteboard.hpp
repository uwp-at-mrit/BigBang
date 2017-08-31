#pragma once

#include "canvas.hpp"
#include "snip/snip.hpp"

namespace Win2D::UIElement {
    public ref class IPasteboard : public Win2DCanvas {
    public:
        virtual ~IPasteboard();

    public:
        property float activeWidth {
            float get() { return float(Control->ActualWidth - Inset.Left - Inset.Right); };
        }

        property float activeHeight {
            float get() { return float(Control->ActualHeight - Inset.Top - Inset.Bottom); };
        }

        property float minWidth {
            void set(float v) { Control->MinWidth = double(v) + Inset.Left + Inset.Right; };
        }

        property float minHeight {
            void set(float v) { Control->MinHeight = double(v) + Inset.Top + Inset.Bottom; };
        }

        property Windows::UI::Xaml::Thickness Inset {
            void set(Windows::UI::Xaml::Thickness v) { padding = v; }
            Windows::UI::Xaml::Thickness get() { return padding; }
        }

    internal:
        IPasteboard(Windows::UI::Xaml::Controls::Panel^ parent, Platform::String^ id);
        IPasteboard(Windows::UI::Xaml::Controls::Panel^ parent, Platform::String^ id, Windows::UI::Xaml::Thickness inset);

    internal:
        void Draw(Microsoft::Graphics::Canvas::CanvasDrawingSession^ ds) override;
    
    internal:
        void Insert(Snip* snip, float x = 0.0, float y = 0.0);
        void MoveTo(Snip* snip, float x, float y);

    private protected:
        virtual void BeforeInsert(Snip* snip, float x, float y) {};
        virtual void AfterInsert(Snip* snip, float x, float y) {};

    private protected:
        Windows::UI::Xaml::Thickness padding;

        Snip* headSnip;
        Snip* tailSnip;
    };

    public ref class Pasteboard sealed : public IPasteboard {
    public:
        Pasteboard(Windows::UI::Xaml::Controls::Panel^ parent, Platform::String^ id);
        Pasteboard(Windows::UI::Xaml::Controls::Panel^ parent, Platform::String^ id, Windows::UI::Xaml::Thickness inset);

    internal:
        void ChangeSize(double width, double height) override;
    };

    public ref class VPasteboard sealed : public IPasteboard {
    public:
        VPasteboard(Windows::UI::Xaml::Controls::Panel^ parent, Platform::String^ id);
        VPasteboard(Windows::UI::Xaml::Controls::Panel^ parent, Platform::String^ id, float gapsize);
        VPasteboard(Windows::UI::Xaml::Controls::Panel^ parent, Platform::String^ id, float gapsize, Windows::UI::Xaml::Thickness inset);

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
