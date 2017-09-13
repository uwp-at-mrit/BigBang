#pragma once

#include "sugar.hpp"
#include "canvas.hxx"

namespace WarGrey::SCADA {
    public value struct TextExtent {
        float width;
        float height;
        float tspace;
        float rspace;
        float bspace;
        float lspace;
    };

    Microsoft::Graphics::Canvas::CanvasDrawingSession^ make_shared_drawing_session();

    TextExtent get_text_extent(
        Platform::String^ message,
        Microsoft::Graphics::Canvas::Text::CanvasTextFormat^ layout_config);

    TextExtent get_text_extent(
        Microsoft::Graphics::Canvas::CanvasDrawingSession^ ds,
        Platform::String^ message,
        Microsoft::Graphics::Canvas::Text::CanvasTextFormat^ layout_config);

    private ref class Win2DCanvas {
    public:
        void begin_edit_sequence();
        void end_edit_sequence();
        void refresh();

    public:
        read_only_property(Microsoft::Graphics::Canvas::UI::Xaml::CanvasControl^, canvas);
        read_only_property(float, actual_width);
        read_only_property(float, actual_height);

        read_write_property(float, canvas_width);
        read_write_property(float, canvas_height);
        read_write_property(float, min_canvas_width);
        read_write_property(float, min_canvas_height);
        read_write_property(float, max_canvas_width);
        read_write_property(float, max_canvas_height);

    internal:
        Win2DCanvas(Windows::UI::Xaml::Controls::Panel^ parent, Platform::String^ id);
    
    internal:
        virtual void resize(double width, double height);
        virtual void load(Microsoft::Graphics::Canvas::UI::CanvasCreateResourcesEventArgs^ args) {};
        virtual void draw(Microsoft::Graphics::Canvas::CanvasDrawingSession^ args) {};

    private protected:
        Microsoft::Graphics::Canvas::UI::Xaml::CanvasControl^ control;
        virtual void on_end_edit_sequence() {};

    private:
        int edit_sequence;
        bool is_refresh_pending;
        
        void do_load(
            Microsoft::Graphics::Canvas::UI::Xaml::CanvasControl^ sender,
            Microsoft::Graphics::Canvas::UI::CanvasCreateResourcesEventArgs^ args);
        
        void do_paint(
            Microsoft::Graphics::Canvas::UI::Xaml::CanvasControl^ sender,
            Microsoft::Graphics::Canvas::UI::Xaml::CanvasDrawEventArgs^ args);
    };
}
