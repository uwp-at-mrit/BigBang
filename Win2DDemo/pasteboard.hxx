#pragma once

#include "canvas.hxx"
#include "snip/snip.hpp"

namespace WarGrey::Win2DDemo {
    ref class Pasteboard;

    private ref class IPasteboardLayout abstract {
    internal:
        virtual void before_insert(Pasteboard^ self, Snip* snip, float x, float y) = 0;
        virtual void after_insert(Pasteboard^ self, Snip* snip, float x, float y) = 0;
    };

    private ref class Pasteboard sealed: public WarGrey::Win2DDemo::Win2DCanvas {
    public:
        Pasteboard(Windows::UI::Xaml::Controls::Panel^ parent, Platform::String^ id);
        Pasteboard(Windows::UI::Xaml::Controls::Panel^ parent, Platform::String^ id, IPasteboardLayout^ layout);
        virtual ~Pasteboard();

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
        void insert(Snip* snip, float x = 0.0, float y = 0.0);
        void move(Snip* snip, float x, float y);

    private protected:
        void recalculate_snips_extent_when_invalid();
        void on_end_edit_sequence() override;

    private protected:
        Windows::UI::Xaml::Thickness padding;
        float snips_x;
        float snips_y;
        float snips_width;
        float snips_height;

    private protected:
        IPasteboardLayout^ layout;
        Snip* head_snip;
        Snip* tail_snip;
    };
}
