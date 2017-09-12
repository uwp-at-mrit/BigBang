#pragma once

#include "sugar.hpp"
#include "canvas.hxx"
#include "forward.hxx"
#include "snip/snip.hpp"
#include "workspace/listener.hpp"

namespace WarGrey::WinACS {
    private ref class Pasteboard sealed: public WarGrey::WinACS::Win2DCanvas {
    public:
        virtual ~Pasteboard();

    public:
        void draw(Microsoft::Graphics::Canvas::CanvasDrawingSession^ ds) override;

    public:
        bool canvas_position_to_drawing_position(float* x, float* y);
        bool drawing_position_to_canvas_position(float* x, float* y);
        void set_preferred_min_size(float width, float height);
        void fill_snips_bounds(float* x, float* y, float* width, float* height);
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
        void set_pointer_listener(WarGrey::WinACS::IPasteboardListener* listener);
        AbstractObject* layout_info = nullptr;

    internal:
        Snip* find_snip(float x, float y);
        void insert(Snip* snip, float x = 0.0, float y = 0.0);
        void move(Snip* snip, float x, float y);
        void move_to(Snip* snip, float x, float y);

    internal:
        void add_selected(Snip* snip);
        void set_selected(Snip* snip);
        void no_selected();

    private protected:
        void recalculate_snips_extent_when_invalid();
        void on_end_edit_sequence() override;

    private:
        WarGrey::WinACS::IPasteboardListener* listener;
        float last_pointer_x;
        float last_pointer_y;
        float rubberband_x[2];
        float* rubberband_y;
        bool rubberband_allowed;

        void on_pointer_moved(Platform::Object^ sender, Windows::UI::Xaml::Input::PointerRoutedEventArgs^ args);
        void on_pointer_pressed(Platform::Object^ sender, Windows::UI::Xaml::Input::PointerRoutedEventArgs^ args);
        void on_pointer_released(Platform::Object^ sender, Windows::UI::Xaml::Input::PointerRoutedEventArgs^ args);
        
    private:
        Windows::UI::Xaml::Thickness padding;
        float snips_left;
        float snips_top;
        float snips_right;
        float snips_bottom;
        float preferred_min_width;
        float preferred_min_height;

    private:
        WarGrey::WinACS::IPasteboardLayout* layout = nullptr;
        WarGrey::WinACS::Snip* head_snip = nullptr;
    };
}
