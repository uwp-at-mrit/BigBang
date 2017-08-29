#pragma once

#include "ui.h"

namespace Win2D::UIElement {
    public value struct TextExtent {
        float Width;
        float Height;
        float Distance;
        float Spacing;
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
        void SmartRedraw();

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
        
        void OnLoad(
            Microsoft::Graphics::Canvas::UI::Xaml::CanvasControl^ sender,
            Microsoft::Graphics::Canvas::UI::CanvasCreateResourcesEventArgs^ args);
        
        void OnPaint(
            Microsoft::Graphics::Canvas::UI::Xaml::CanvasControl^ sender,
            Microsoft::Graphics::Canvas::UI::Xaml::CanvasDrawEventArgs^ args);
    };

    /*********************************************************************************************/
    private enum SnipTypes {
        Text
    };

    private class Snip {
    public:
        virtual ~Snip() {};

    public:
        virtual SnipTypes GetType() = 0;
        virtual TextExtent GetExtent(Microsoft::Graphics::Canvas::CanvasDrawingSession^ ds, double x, double y) = 0;
        virtual void Draw(Microsoft::Graphics::Canvas::CanvasDrawingSession^ ds, double x, double y) = 0;

    public:
        Snip* next;
        Snip* prev;
    };

    /*********************************************************************************************/
    public ref class Pasteboard sealed : public Win2DCanvas {
    public:
        Pasteboard(Windows::UI::Xaml::Controls::Panel^ parent, Platform::String^ id);

    internal:
        void Draw(Microsoft::Graphics::Canvas::CanvasDrawingSession^ ds) override;
    
    internal:
        void Insert(Snip* snip);
        void Insert(Snip* snip, double x, double y);

    private:
        Snip* headSnip;
        ~Pasteboard();
    };
}
