#pragma once

#include "canvas.hxx"
#include "snip/snip.hpp"

namespace Win2D::UIElement {
    public ref class IPasteboard : public Win2DCanvas {
    public:
        virtual ~IPasteboard();

    public:
        property float active_width {
            float get() { return float(canvas->ActualWidth - inset.Left - inset.Right); };
        }

        property float active_height {
            float get() { return float(canvas->ActualHeight - inset.Top - inset.Bottom); };
        }

        property float min_width {
            void set(float v) { canvas->MinWidth = double(v) + inset.Left + inset.Right; };
        }

        property float min_height {
            void set(float v) { canvas->MinHeight = double(v) + inset.Top + inset.Bottom; };
        }

        property Windows::UI::Xaml::Thickness inset {
            void set(Windows::UI::Xaml::Thickness v) { padding = v; }
            Windows::UI::Xaml::Thickness get() { return padding; }
        }

    internal:
        IPasteboard(Windows::UI::Xaml::Controls::Panel^ parent, Platform::String^ id);
        IPasteboard(Windows::UI::Xaml::Controls::Panel^ parent, Platform::String^ id, Windows::UI::Xaml::Thickness inset);

    internal:
        void draw(Microsoft::Graphics::Canvas::CanvasDrawingSession^ ds) override;

        /*
        bool action(float x, float y,
            Windows::UI::Input::PointerPointProperties^ ppps,
            Windows::System::VirtualKeyModifiers vkms,
            Windows::Devices::Input::PointerDeviceType type) override;
        */

    internal:
        void insert(Snip* snip, float x = 0.0, float y = 0.0);
        void move(Snip* snip, float x, float y);

    private protected:
        virtual void before_insert(Snip* snip, float x, float y) {};
        virtual void after_insert(Snip* snip, float x, float y) {};

    private protected:
        Windows::UI::Xaml::Thickness padding;

        Snip* head_snip;
        Snip* tail_snip;
    };

    public ref class Pasteboard sealed : public IPasteboard {
    public:
        Pasteboard(Windows::UI::Xaml::Controls::Panel^ parent, Platform::String^ id);
        Pasteboard(Windows::UI::Xaml::Controls::Panel^ parent, Platform::String^ id, Windows::UI::Xaml::Thickness inset);

    internal:
        void resize(double width, double height) override;
    };

    public ref class VPasteboard sealed : public IPasteboard {
    public:
        VPasteboard(Windows::UI::Xaml::Controls::Panel^ parent, Platform::String^ id);
        VPasteboard(Windows::UI::Xaml::Controls::Panel^ parent, Platform::String^ id, float gapsize);
        VPasteboard(Windows::UI::Xaml::Controls::Panel^ parent, Platform::String^ id, float gapsize, Windows::UI::Xaml::Thickness inset);

    internal:
        void resize(double width, double height) override;

    private protected:
        void before_insert(Snip* snip, float x, float y) override;
        void after_insert(Snip* snip, float x, float y) override;

    private:
        float gapsize;
        float anchor;
    };
}
