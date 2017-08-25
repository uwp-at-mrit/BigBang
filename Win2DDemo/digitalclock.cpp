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

DigitalClock::DigitalClock(Panel^ parent) : Win2DPanel(parent, "SystemClock") {
    auto onTick = ref new ObjectHandler(this, &DigitalClock::OnTickUpdate);

    longdate = ref new DateTimeFormatter("longdate");
    longtime = ref new DateTimeFormatter("longtime");
    datetime = ref new Calendar();
    fontInfo = ref new CanvasTextFormat();

    fontInfo->WordWrapping = CanvasWordWrapping::NoWrap;
    fontInfo->FontSize = 12;

    timer = gui_timer(0, onTick);
}

void DigitalClock::UpdateTimeStamp() {
    datetime->SetToNow();
    long long l00ns = datetime->Nanosecond / 100;
    long long ms = l00ns / 10000;
    timer->Interval = TimeSpan({ 10000000 - l00ns });
    datestamp = longdate->Format(datetime->GetDateTime());
    timestamp = longtime->Format(datetime->GetDateTime())
        + ((ms < 10) ? ".00" : ((ms < 100) ? ".0" : "."))
        + ms.ToString();
}

void DigitalClock::OnTickUpdate(Object^ sender, Object^ e) {
    UpdateTimeStamp();
    this->SmartRedraw();
}

void DigitalClock::LoadResources(CanvasCreateResourcesEventArgs^ e) {
    if (timestamp == nullptr) {
        UpdateTimeStamp();
    }
}

void DigitalClock::Draw(CanvasDrawingSession^ ds) {
    auto timeLayout = ref new CanvasTextLayout(ds, timestamp, fontInfo, 0.0f, 0.0f);
    auto dateLayout = ref new CanvasTextLayout(ds, datestamp, fontInfo, 0.0f, 0.0f);
    float width = (float)Width;
    float height = (float)Height;

    bool isTimestampLonger = (timeLayout->LayoutBounds.Width > dateLayout->LayoutBounds.Width);
    float deltaWidth = abs(dateLayout->LayoutBounds.Width - timeLayout->LayoutBounds.Width) / 2.0f;

    float tx = width - timeLayout->LayoutBounds.Width - (isTimestampLonger ? 0.0f : deltaWidth);
    float ty = (height - timeLayout->LayoutBounds.Height - dateLayout->LayoutBounds.Height) / 2.0f;
    ds->DrawTextLayout(timeLayout, tx, ty, Colors::Black);

    float dx = width - dateLayout->LayoutBounds.Width - (isTimestampLonger ? deltaWidth : 0);
    float dy = ty + dateLayout->LayoutBounds.Height;
    ds->DrawTextLayout(dateLayout, dx, dy, Colors::Black);
}
