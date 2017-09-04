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
        read_write_property(Windows::UI::Xaml::Thickness, inset);

        read_write_property(float, layer_width);
        read_write_property(float, layer_height);

        read_write_property(float, min_layer_width);
        read_write_property(float, min_layer_height);

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
        void on_end_edit_sequence() override;

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
