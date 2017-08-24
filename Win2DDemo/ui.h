#pragma once

namespace WUX = Windows::UI::Xaml;
namespace WUC = WUX::Controls;
namespace MSUI = Microsoft::Graphics::Canvas::UI;
namespace MSX = MSUI::Xaml;

typedef Windows::Foundation::EventHandler<Platform::Object^> ObjectHandler;
typedef Windows::Foundation::TypedEventHandler<MSX::CanvasControl^, MSX::CanvasDrawEventArgs^> CanvasDrawHandler;
typedef Windows::Foundation::TypedEventHandler<MSX::CanvasControl^, MSUI::CanvasCreateResourcesEventArgs^> CanvasLoadHandler;

namespace Win2D::Xaml {
    public ref class XAML sealed {
    public:
       static WUC::StackPanel^ MakeStackPanel(
           WUC::Panel^ parent,
           WUC::Orientation direction,
           WUX::Thickness margin,
           WUX::Thickness padding);

       static WUC::ToggleSwitch^ MakeToggleSwitch(
           WUC::Panel^ parent,
           Platform::String^ id,
           Platform::String^ onCaption,
           Platform::String^ offCaption);

       static MSX::CanvasControl^ MakeGPUCanvas(
           WUC::Panel^ parent,
           Platform::String^ id,
           CanvasLoadHandler^ Load,
           CanvasDrawHandler^ Draw);

       static WUC::Canvas^ MakeCanvas(
           WUC::Panel^ parent,
           Platform::String^ id);

       static WUX::DispatcherTimer^ MakeGUITimer(
           long long ms,
           ObjectHandler^ handler);
    };

    public ref class Win2DPanel : public WUX::DependencyObject {
    public:
        void ChangeSize(double width, double height);
        void SmartRedraw();

    public:
        property MSX::CanvasControl^ Canvas { MSX::CanvasControl^ get() { return entity; } }
        property double Width { double get() { return entity->Width; } }
        property double Height { double get() { return entity->Height; } }

    internal:
        Win2DPanel(WUC::Panel^ parent, Platform::String^ id);

        virtual void OnDisplaySize(double width, double height) {};
        virtual void LoadResources(MSX::CanvasControl^ sender, MSUI::CanvasCreateResourcesEventArgs^ args) {};
        virtual void Draw(MSX::CanvasControl^ sender, Microsoft::Graphics::Canvas::CanvasDrawingSession^ args) {};

    private:
        MSX::CanvasControl^ entity;

        void OnLoad(MSX::CanvasControl^ sender, MSUI::CanvasCreateResourcesEventArgs^ args);
        void OnPaint(MSX::CanvasControl^ sender, MSX::CanvasDrawEventArgs^ args);
    };

    public ref class DigitalClock sealed : public Win2DPanel {
    public:
        DigitalClock(WUC::Panel^ parent);

    internal:
        void LoadResources(MSX::CanvasControl^ sender, MSUI::CanvasCreateResourcesEventArgs^ args) override;
        void Draw(MSX::CanvasControl^ sender, Microsoft::Graphics::Canvas::CanvasDrawingSession^ args) override;

    private:
        void UpdateTimeStamp();
        void OnTickUpdate(Object^ sender, Object^ e);

    private:
        Platform::String^ timestamp;
        Platform::String^ datestamp;
        Microsoft::Graphics::Canvas::Text::CanvasTextFormat^ fontInfo;

    private:
        WUX::DispatcherTimer^ timer;
        Windows::Globalization::Calendar^ datetime;
        Windows::Globalization::DateTimeFormatting::DateTimeFormatter^ longdate;
        Windows::Globalization::DateTimeFormatting::DateTimeFormatter^ longtime;
    };

    public ref class Pasteboard sealed : public Win2DPanel {
    public:
        Pasteboard(WUC::Panel^ parent, Platform::String^ id);

    private:

    };
}
