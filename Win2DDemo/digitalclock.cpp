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

#include "debug.h"
DigitalClock::DigitalClock(Panel^ parent) : Win2DCanvas(parent, "SystemClock") {
    auto onTick = ref new ObjectHandler(this, &DigitalClock::OnTickUpdate);

    longdate = ref new DateTimeFormatter("longdate");
    longtime = ref new DateTimeFormatter("longtime");
    datetime = ref new Calendar();
    fontInfo = ref new CanvasTextFormat();

    fontInfo->WordWrapping = CanvasWordWrapping::NoWrap;
    fontInfo->FontSize = 12;

    timer = gui_timer(1000, onTick);
    UpdateTimeStamp();
}

void DigitalClock::UpdateTimeStamp() {
    datetime->SetToNow();
    long long l00ns = datetime->Nanosecond / 100;
    long long ms = l00ns / 10000;
    timer->Interval = TimeSpan({ 10000000 - l00ns });
    timestamp = longtime->Format(datetime->GetDateTime())
        + ((ms < 10) ? ".00" : ((ms < 100) ? ".0" : "."))
        + ms.ToString()
        + "\n"
        + longdate->Format(datetime->GetDateTime());
}

void DigitalClock::OnTickUpdate(Object^ sender, Object^ e) {
    UpdateTimeStamp();
    this->SmartRedraw();
}

void DigitalClock::Draw(CanvasDrawingSession^ ds) {
    auto layout = ref new CanvasTextLayout(ds, timestamp, fontInfo, 0.0f, 0.0f);
    layout->HorizontalAlignment = CanvasHorizontalAlignment::Center;

    float x = ((float)Width) - layout->LayoutBounds.Width - layout->LayoutBounds.X;
    float y = ((float)Height - layout->LayoutBounds.Height) / 2.0f;
    ds->DrawTextLayout(layout, x, y, Colors::Black);
}
