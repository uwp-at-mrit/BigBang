#pragma once

#include "listener/pointer.hxx"

namespace WarGrey::Win2DDemo {
    public value struct TextExtent {
        float width;
        float height;
        float descent;
        float space;
        float lspace;
        float rspace;
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
        void set_pointer_lisener(WarGrey::Win2DDemo::IPointerListener^ listener);
        virtual bool canvas_position_to_drawing_position(float* x, float* y) { return true; };
        virtual bool drawing_position_to_canvas_position(float* x, float* y) { return true; };

    public:
        property Microsoft::Graphics::Canvas::UI::Xaml::CanvasControl^ canvas {
            Microsoft::Graphics::Canvas::UI::Xaml::CanvasControl^ get() { return control; }
        }

    internal:
        Win2DCanvas(Windows::UI::Xaml::Controls::Panel^ parent, Platform::String^ id);
    
    internal:
        virtual void resize(double width, double height) {};
        virtual void load(Microsoft::Graphics::Canvas::UI::CanvasCreateResourcesEventArgs^ args) {};
        virtual void draw(Microsoft::Graphics::Canvas::CanvasDrawingSession^ args) {};

    private:
        Microsoft::Graphics::Canvas::UI::Xaml::CanvasControl^ control;
        Windows::UI::Input::PointerPointProperties^ ppps;
        WarGrey::Win2DDemo::IPointerListener^ listener;

        int edit_sequence;
        bool is_refresh_pending;
        
        void do_load(
            Microsoft::Graphics::Canvas::UI::Xaml::CanvasControl^ sender,
            Microsoft::Graphics::Canvas::UI::CanvasCreateResourcesEventArgs^ args);
        
        void do_paint(
            Microsoft::Graphics::Canvas::UI::Xaml::CanvasControl^ sender,
            Microsoft::Graphics::Canvas::UI::Xaml::CanvasDrawEventArgs^ args);

        void do_click(
            Platform::Object^ sender,
            Windows::UI::Xaml::Input::PointerRoutedEventArgs^ args);

        void do_notice(
            Platform::Object^ sender,
            Windows::UI::Xaml::Input::PointerRoutedEventArgs^ args);

        void delay_pressed(
            Platform::Object^ sender,
            Windows::UI::Xaml::Input::PointerRoutedEventArgs^ args);
    };
}
