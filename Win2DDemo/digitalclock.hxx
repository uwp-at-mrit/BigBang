#pragma once

#include "canvas.hxx"

namespace Win2DDemo {
    private ref class DigitalClock sealed : public Win2DDemo::Win2DCanvas {
    public:
        DigitalClock(Windows::UI::Xaml::Controls::Panel^ parent);

    internal:
        void resize(double width, double height) override;
        void draw(Microsoft::Graphics::Canvas::CanvasDrawingSession^ ds) override;
        
    private:
        void update_timestamp();
        void do_update_on_tick(Platform::Object^ sender, Platform::Object^ e);

    private:
        Platform::String^ timestamp;
        Microsoft::Graphics::Canvas::Text::CanvasTextFormat^ layout_config;

    private:
        Windows::UI::Xaml::DispatcherTimer^ timer;
        Windows::Globalization::Calendar^ datetime;
        Windows::Globalization::DateTimeFormatting::DateTimeFormatter^ longdate;
        Windows::Globalization::DateTimeFormatting::DateTimeFormatter^ longtime;
    };
}
