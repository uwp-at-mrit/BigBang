#include <cmath>

#include "ui.h"
#include "digitalclock.h"

using namespace std;
using namespace Platform;

using namespace Win2D::UIElement;

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
    auto onTick = ref new ObjectHandler(this, &DigitalClock::OnTickUpdate);

    this->longdate = ref new DateTimeFormatter("longdate");
    this->longtime = ref new DateTimeFormatter("longtime");
    this->datetime = ref new Calendar();
    this->fontInfo = ref new CanvasTextFormat();

    this->fontInfo->WordWrapping = CanvasWordWrapping::NoWrap;
    this->fontInfo->FontSize = 12;

    this->timer = gui_timer(1000, onTick);
    this->UpdateTimeStamp();
}

void DigitalClock::UpdateTimeStamp() {
    datetime->SetToNow();
    long long l00ns = datetime->Nanosecond / 100;
    long long ms = l00ns / 10000;
    this->timer->Interval = TimeSpan({ 10000000 - l00ns });
    this->timestamp = this->longtime->Format(datetime->GetDateTime())
        + ((ms < 10) ? ".00" : ((ms < 100) ? ".0" : "."))
        + ms.ToString()
        + "\n"
        + this->longdate->Format(datetime->GetDateTime());
}

void DigitalClock::OnTickUpdate(Object^ sender, Object^ e) {
    this->UpdateTimeStamp();
    this->Refresh();
}

void DigitalClock::Draw(CanvasDrawingSession^ ds) {
    auto layout = ref new CanvasTextLayout(ds, timestamp, fontInfo, 0.0f, 0.0f);
    layout->HorizontalAlignment = CanvasHorizontalAlignment::Center;

    float x = ((float)Control->Width) - layout->LayoutBounds.Width - layout->LayoutBounds.X;
    float y = ((float)Control->Height - layout->LayoutBounds.Height) / 2.0f;
    ds->DrawTextLayout(layout, x, y, Colors::Black);
}

void DigitalClock::ChangeSize(double width, double height) {
    this->Control->Width = width;
    this->Control->Height = height;
}
