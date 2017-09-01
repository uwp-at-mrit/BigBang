#pragma once

typedef bool (*CanvasInputHandler)(
    float x, float y,
    Windows::UI::Input::PointerPointProperties ppps,
    Windows::System::VirtualKeyModifiers vkms);

namespace Win2D::UIElement {
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

    public ref class Win2DCanvas : public Windows::UI::Xaml::DependencyObject {
    public:
        void begin_edit_sequence();
        void end_edit_sequence();
        void refresh();

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

    internal:
        virtual bool point(
            float x, float y,
            Windows::UI::Input::PointerPointProperties^ ppps,
            Windows::System::VirtualKeyModifiers vkms);

        virtual bool touch(
            float x, float y,
            Windows::UI::Input::PointerPointProperties^ ppps,
            Windows::System::VirtualKeyModifiers vkms);

        virtual bool paint(float x, float y,
            Windows::UI::Input::PointerPointProperties^ ppps,
            Windows::System::VirtualKeyModifiers vkms);

    private:
        Microsoft::Graphics::Canvas::UI::Xaml::CanvasControl^ control;

        int edit_sequence;
        bool is_refresh_pending;
        
        void do_load(
            Microsoft::Graphics::Canvas::UI::Xaml::CanvasControl^ sender,
            Microsoft::Graphics::Canvas::UI::CanvasCreateResourcesEventArgs^ args);
        
        void do_display(
            Microsoft::Graphics::Canvas::UI::Xaml::CanvasControl^ sender,
            Microsoft::Graphics::Canvas::UI::Xaml::CanvasDrawEventArgs^ args);

        void do_input(
            Platform::Object^ sender,
            Windows::UI::Xaml::Input::PointerRoutedEventArgs^ args);
    };
}
