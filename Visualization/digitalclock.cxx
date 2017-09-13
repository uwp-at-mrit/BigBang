#include <cmath>

#include "ui.hxx"
#include "digitalclock.hxx"

using namespace WarGrey::SCADA;

using namespace Windows::Foundation;
using namespace Windows::Globalization;
using namespace Windows::Globalization::DateTimeFormatting;

using namespace Windows::UI;
using namespace Windows::UI::Xaml;
using namespace Windows::UI::Xaml::Controls;

using namespace Microsoft::Graphics::Canvas;
using namespace Microsoft::Graphics::Canvas::Text;
using namespace Microsoft::Graphics::Canvas::UI;
using namespace Microsoft::Graphics::Canvas::UI::Xaml;

DigitalClock::DigitalClock(Panel^ parent) : Win2DCanvas(parent, "SystemClock") {
    auto do_update = ref new ObjectHandler(this, &DigitalClock::do_update_on_tick);

    this->longdate = ref new DateTimeFormatter("longdate");
    this->longtime = ref new DateTimeFormatter("longtime");
    this->datetime = ref new Calendar();
    this->layout_config = ref new CanvasTextFormat();

    this->layout_config->WordWrapping = CanvasWordWrapping::NoWrap;
    this->layout_config->FontSize = 12;

    this->timer = gui_timer(1000, do_update);
    this->update_timestamp();
}

void DigitalClock::update_timestamp() {
    datetime->SetToNow();
    long long l00ns = datetime->Nanosecond / 100;
    long long ms = l00ns / 10000;
    this->timer->Interval = TimeSpan{ 10000000 - l00ns };
    this->timestamp = this->longtime->Format(datetime->GetDateTime())
        + ((ms < 10) ? ".00" : ((ms < 100) ? ".0" : "."))
        + ms.ToString()
        + "\n"
        + this->longdate->Format(datetime->GetDateTime());
}

void DigitalClock::do_update_on_tick(Object^ sender, Object^ e) {
    this->update_timestamp();
    this->refresh();
}

void DigitalClock::draw(CanvasDrawingSession^ ds) {
    auto layout = ref new CanvasTextLayout(ds, timestamp, layout_config, 0.0f, 0.0f);
    layout->HorizontalAlignment = CanvasHorizontalAlignment::Center;

    float x = this->canvas_width - layout->LayoutBounds.Width - layout->LayoutBounds.X;
    float y = (this->canvas_height - layout->LayoutBounds.Height) / 2.0f;

    ds->DrawTextLayout(layout, x, y, Colors::Black);
}
