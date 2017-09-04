#pragma once

#include "canvas.hxx"
#include "snip/snip.hpp"

namespace WarGrey::Win2DDemo {
    private ref class IPasteboard : public WarGrey::Win2DDemo::Win2DCanvas {
    public:
        virtual ~IPasteboard();

    public:
        void draw(Microsoft::Graphics::Canvas::CanvasDrawingSession^ ds) override;

    public:
        bool canvas_position_to_drawing_position(float* x, float* y) override;
        bool drawing_position_to_canvas_position(float* x, float* y) override;
        void fill_snips_extent(float* x, float* y, float* width, float* height);
        void size_cache_invalid();

    public:
        property Windows::UI::Xaml::Thickness inset {
            void set(Windows::UI::Xaml::Thickness v) { padding = v; }
            Windows::UI::Xaml::Thickness get() { return padding; }
        }

        property float min_canvas_width {
            void set(float v) { canvas->MinWidth = double(v); }
            float get() { return float(canvas->MinWidth); };
        }
        
        property float min_canvas_height {
            void set(float v) { canvas->MinHeight = double(v); }
            float get() { return float(canvas->MinHeight); };
        }

        property float canvas_width {
            void set(float v) { canvas->Width = double(v); }
            float get() { return float(canvas->ActualWidth); };
        }

        property float canvas_height {
            void set(float v) { canvas->Height = double(v); }
            float get() { return float(canvas->ActualHeight); };
        }

        property float min_layer_width {
            void set(float v) { canvas->MinWidth = double(v) + inset.Left + inset.Right; };
            float get() { return float(canvas->MinWidth - inset.Left - inset.Right); };
        }

        property float min_layer_height {
            void set(float v) { canvas->MinHeight = double(v) + inset.Top + inset.Bottom; };
            float get() { return float(canvas->MinHeight - inset.Top - inset.Bottom); };
        }

        property float layer_width {
            void set(float v) { canvas->Height = double(v) + inset.Top + inset.Bottom; };
            float get() { return float(canvas->ActualWidth - inset.Left - inset.Right); };
        }

        property float layer_height {
            void set(float v) { canvas->Height = double(v) + inset.Top + inset.Bottom; };
            float get() { return float(canvas->ActualHeight - inset.Top - inset.Bottom); };
        }

    internal:
        IPasteboard(Windows::UI::Xaml::Controls::Panel^ parent, Platform::String^ id);
        IPasteboard(Windows::UI::Xaml::Controls::Panel^ parent, Platform::String^ id, Windows::UI::Xaml::Thickness inset);

    internal:
        void insert(Snip* snip, float x = 0.0, float y = 0.0);
        void move(Snip* snip, float x, float y);

    private protected:
        virtual void before_insert(Snip* snip, float x, float y) {};
        virtual void after_insert(Snip* snip, float x, float y) {};
        virtual void recalculate_snips_extent_when_invalid();

    private protected:
        Windows::UI::Xaml::Thickness padding;
        float snips_x;
        float snips_y;
        float snips_width;
        float snips_height;

    private protected:
        Snip* head_snip;
        Snip* tail_snip;
    };

    private ref class Pasteboard sealed : public WarGrey::Win2DDemo::IPasteboard {
    public:
        Pasteboard(Windows::UI::Xaml::Controls::Panel^ parent, Platform::String^ id);
        Pasteboard(Windows::UI::Xaml::Controls::Panel^ parent, Platform::String^ id, Windows::UI::Xaml::Thickness inset);
    };

    private ref class VPasteboard sealed : public WarGrey::Win2DDemo::IPasteboard {
    public:
        VPasteboard(Windows::UI::Xaml::Controls::Panel^ parent, Platform::String^ id);
        VPasteboard(Windows::UI::Xaml::Controls::Panel^ parent, Platform::String^ id, float gapsize);
        VPasteboard(Windows::UI::Xaml::Controls::Panel^ parent, Platform::String^ id, float gapsize, Windows::UI::Xaml::Thickness inset);

    private protected:
        void before_insert(Snip* snip, float x, float y) override;
        void after_insert(Snip* snip, float x, float y) override;

    private:
        float gapsize;
        float anchor;
    };
}
