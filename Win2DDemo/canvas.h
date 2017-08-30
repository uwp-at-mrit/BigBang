#pragma once

namespace Win2D::UIElement {
    public value struct TextExtent {
        float Width;
        float Height;
        float Descent;
        float Space;
        float LSpace;
        float RSpace;
    };

    public ref class Win2DCanvas : public Windows::UI::Xaml::DependencyObject {
    public:
        static TextExtent GetTextExtent(
            Platform::String^ message,
            Microsoft::Graphics::Canvas::Text::CanvasTextFormat^ fontInfo);

        static TextExtent GetTextExtent(
            Microsoft::Graphics::Canvas::CanvasDrawingSession^ ds,
            Platform::String^ message,
            Microsoft::Graphics::Canvas::Text::CanvasTextFormat^ fontInfo);

    public:
        void ChangeSize(double width, double height);

    public:
        void BeginEditSequence();
        void EndEditSequence();
        void Refresh();

    public:
        property Microsoft::Graphics::Canvas::UI::Xaml::CanvasControl^ Control {
            Microsoft::Graphics::Canvas::UI::Xaml::CanvasControl^ get() { return control; }
        }

    internal:
        Win2DCanvas(Windows::UI::Xaml::Controls::Panel^ parent, Platform::String^ id);

    internal:
        virtual void OnDisplaySize(double width, double height) {};
        virtual void LoadResources(Microsoft::Graphics::Canvas::UI::CanvasCreateResourcesEventArgs^ args) {};
        virtual void Draw(Microsoft::Graphics::Canvas::CanvasDrawingSession^ args) {};

    private:
        Microsoft::Graphics::Canvas::UI::Xaml::CanvasControl^ control;
        int editSequence;
        bool isRefreshPending;
        
        void OnLoad(
            Microsoft::Graphics::Canvas::UI::Xaml::CanvasControl^ sender,
            Microsoft::Graphics::Canvas::UI::CanvasCreateResourcesEventArgs^ args);
        
        void OnPaint(
            Microsoft::Graphics::Canvas::UI::Xaml::CanvasControl^ sender,
            Microsoft::Graphics::Canvas::UI::Xaml::CanvasDrawEventArgs^ args);
    };
}
