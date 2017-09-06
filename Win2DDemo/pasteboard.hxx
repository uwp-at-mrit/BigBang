#pragma once

#include "canvas.hxx"
#include "snip/snip.hpp"

namespace WarGrey::Win2DDemo {
    ref class Pasteboard;

    private class IPasteboardLayout {
    public:
        virtual void on_attach_to(Pasteboard^ self) {};
        virtual ~IPasteboardLayout() noexcept {};

    public:
        virtual void before_insert(Pasteboard^ self, Snip* snip, float x, float y) = 0;
        virtual void after_insert(Pasteboard^ self, Snip* snip, float x, float y) = 0;

    public:
        int refcount = 0;
    };

    private ref class Pasteboard sealed: public WarGrey::Win2DDemo::Win2DCanvas {
    public:
        virtual ~Pasteboard();

    public:
        void draw(Microsoft::Graphics::Canvas::CanvasDrawingSession^ ds) override;

    public:
        bool canvas_position_to_drawing_position(float* x, float* y) override;
        bool drawing_position_to_canvas_position(float* x, float* y) override;
        void set_preferred_min_size(float width, float height);
        void fill_snips_extent(float* width, float* height, float* x = nullptr, float* y = nullptr);
        void size_cache_invalid();

    public:
        read_write_property(Windows::UI::Xaml::Thickness, inset);

        read_write_property(float, layer_width);
        read_write_property(float, layer_height);

        read_write_property(float, min_layer_width);
        read_write_property(float, min_layer_height);

        read_write_property(float, max_layer_width);
        read_write_property(float, max_layer_height);

        read_only_property(float, actual_layer_width);
        read_only_property(float, actual_layer_height);

    internal:
        Pasteboard(Windows::UI::Xaml::Controls::Panel^ parent, Platform::String^ id, IPasteboardLayout* layout = nullptr);
        void* layout_info;

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
        float preferred_min_width;
        float preferred_min_height;

    private protected:
        IPasteboardLayout* layout;
        Snip* head_snip;
        Snip* tail_snip;
    };
}
