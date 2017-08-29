#pragma once

#include "canvas.h"

namespace Win2D::UIElement {
    public ref class DigitalClock sealed : public Win2D::UIElement::Win2DCanvas {
    public:
        DigitalClock(Windows::UI::Xaml::Controls::Panel^ parent);

    internal:
        void Draw(Microsoft::Graphics::Canvas::CanvasDrawingSession^ ds) override;
        
    private:
        void UpdateTimeStamp();
        void OnTickUpdate(Platform::Object^ sender, Platform::Object^ e);

    private:
        Platform::String^ timestamp;
        Microsoft::Graphics::Canvas::Text::CanvasTextFormat^ fontInfo;

    private:
        Windows::UI::Xaml::DispatcherTimer^ timer;
        Windows::Globalization::Calendar^ datetime;
        Windows::Globalization::DateTimeFormatting::DateTimeFormatter^ longdate;
        Windows::Globalization::DateTimeFormatting::DateTimeFormatter^ longtime;
    };
}
