#pragma once

#include "ui.h"
#include "snip.h"

namespace Win2D::UIElement {
    public ref class Win2DPanel : public Windows::UI::Xaml::DependencyObject {
    public:
        void ChangeSize(double width, double height);
        void SmartRedraw();

    public:
        property Microsoft::Graphics::Canvas::UI::Xaml::CanvasControl^ Canvas {
            Microsoft::Graphics::Canvas::UI::Xaml::CanvasControl^ get() { return entity; }
        }

        property double Width {
            double get() { return entity->Width; }
        }

        property double Height {
            double get() { return entity->Height; }
        }

    internal:
        Win2DPanel(Windows::UI::Xaml::Controls::Panel^ parent, Platform::String^ id);

    internal:
        virtual void OnDisplaySize(double width, double height) {};
        virtual void LoadResources(Microsoft::Graphics::Canvas::UI::CanvasCreateResourcesEventArgs^ args) {};
        virtual void Draw(Microsoft::Graphics::Canvas::CanvasDrawingSession^ args) {};

    private:
        Microsoft::Graphics::Canvas::UI::Xaml::CanvasControl^ entity;

        void OnLoad(
            Microsoft::Graphics::Canvas::UI::Xaml::CanvasControl^ sender,
            Microsoft::Graphics::Canvas::UI::CanvasCreateResourcesEventArgs^ args);
        
        void OnPaint(
            Microsoft::Graphics::Canvas::UI::Xaml::CanvasControl^ sender,
            Microsoft::Graphics::Canvas::UI::Xaml::CanvasDrawEventArgs^ args);
    };

    public ref class Pasteboard sealed : public Win2DPanel {
    public:
        Pasteboard(Windows::UI::Xaml::Controls::Panel^ parent, Platform::String^ id);

    internal:
        virtual void LoadResources(Microsoft::Graphics::Canvas::UI::CanvasCreateResourcesEventArgs^ args) override;
        virtual void Draw(Microsoft::Graphics::Canvas::CanvasDrawingSession^ ds) override;
    
    internal:
        void Insert(Win2D::UIElement::Snip* snip);

    private:
        Win2D::UIElement::Snip* headSnip;
        ~Pasteboard();
    };
}
