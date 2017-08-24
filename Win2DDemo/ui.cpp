#include <cmath>

#include "ui.h"

using namespace Win2D::Xaml;
using namespace Platform;

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

template <class T>
static T PlaceUIElement(Panel^ parent, T child, String^ id) {
    if (id != nullptr)  child->Name = id;
    parent->Children->Append(child);

    return child;
}

StackPanel^ XAML::MakeStackPanel(Panel^ parent, Orientation direction, Thickness margin, Thickness padding) {
    auto child = ref new StackPanel();

    child->Orientation = direction;
    child->Margin = margin;
    child->Padding = padding;

    return PlaceUIElement<StackPanel^>(parent, child, nullptr);
}

ToggleSwitch^ XAML::MakeToggleSwitch(Panel^ parent, String^ id, String^ onCaption, String^ offCaption) {
    auto child = ref new ToggleSwitch();

    child->OnContent = (onCaption == nullptr) ? ((id == nullptr) ? "" : id) : onCaption;
    child->OffContent = (offCaption == nullptr) ? child->OnContent : offCaption;
    
    return PlaceUIElement<ToggleSwitch^>(parent, child, id);
}

CanvasControl^ XAML::MakeGPUCanvas(Panel^ parent, String^ id, CanvasLoadHandler^ OnLoad, CanvasDrawHandler^ OnDraw) {
    auto child = ref new CanvasControl();

    child->CreateResources += OnLoad;
    child->Draw += OnDraw;

    return PlaceUIElement<CanvasControl^>(parent, child, id);
}


Canvas^ XAML::MakeCanvas(Panel^ parent, String^ id) {
    auto child = ref new Canvas();

    return PlaceUIElement<Canvas^>(parent, child, id);
}

DispatcherTimer^ XAML::MakeGUITimer(long long ms, ObjectHandler^ OnTick) {
    auto timer = ref new DispatcherTimer();
    long long duration = ms * 1000 * 10;

    timer->Interval = TimeSpan({ duration });
    timer->Start();
    timer->Tick += OnTick;

    return timer;
}

/*************************************************************************************************/
Win2DPanel::Win2DPanel(Panel^ parent, String^ id) {
    auto onLoad = ref new CanvasLoadHandler(this, &Win2DPanel::OnLoad);
    auto onPaint = ref new CanvasDrawHandler(this, &Win2DPanel::OnPaint);
    entity = XAML::MakeGPUCanvas(parent, id, onLoad, onPaint);
}

void Win2DPanel::OnLoad(CanvasControl^ sender, CanvasCreateResourcesEventArgs^ e) {
    this->LoadResources(sender, e);
}

void Win2DPanel::OnPaint(CanvasControl^ sender, CanvasDrawEventArgs^ e) {
    this->Draw(sender, e->DrawingSession);
}

void Win2DPanel::ChangeSize(double width, double height) {
    entity->Width = width;
    entity->Height = height;
    this->OnDisplaySize(width, height);
}

void Win2DPanel::SmartRedraw() {
    entity->Invalidate();
}

/*************************************************************************************************/
Pasteboard::Pasteboard(Panel^ parent, String^ id) : Win2DPanel(parent, id) {

}

/*************************************************************************************************/
DigitalClock::DigitalClock(Panel^ parent) : Win2DPanel(parent, "SystemClock") {
    auto onTick = ref new ObjectHandler(this, &DigitalClock::OnTickUpdate);

    longdate = ref new DateTimeFormatter("longdate");
    longtime = ref new DateTimeFormatter("longtime");
    datetime = ref new Calendar();
    timer = XAML::MakeGUITimer(0, onTick);
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

void DigitalClock::LoadResources(CanvasControl^ sender, CanvasCreateResourcesEventArgs^ e) {
    if (timestamp == nullptr) {
        UpdateTimeStamp();
    }

    if (fontInfo == nullptr) {
        fontInfo = ref new CanvasTextFormat();
        fontInfo->WordWrapping = CanvasWordWrapping::NoWrap;
        fontInfo->FontSize = 12;
    }
}

void DigitalClock::Draw(CanvasControl^ sender, CanvasDrawingSession^ ds) {
    CanvasTextLayout^ lytTime = ref new CanvasTextLayout(ds, timestamp, fontInfo, 0.0f, 0.0f);
    CanvasTextLayout^ lytDate = ref new CanvasTextLayout(ds, datestamp, fontInfo, 0.0f, 0.0f);
    float width = (float)Width;
    float height = (float)Height;

    bool isTimestampLonger = (lytTime->LayoutBounds.Width > lytDate->LayoutBounds.Width);
    float deltaWidth = abs(lytDate->LayoutBounds.Width - lytTime->LayoutBounds.Width) / 2.0f;
    
    float tx = width - lytTime->LayoutBounds.Width - (isTimestampLonger ? 0.0f : deltaWidth);
    float ty = (height - lytTime->LayoutBounds.Height - lytDate->LayoutBounds.Height) / 2.0f;
    ds->DrawTextLayout(lytTime, tx, ty, Colors::Black);
    
    float dx = width - lytDate->LayoutBounds.Width - (isTimestampLonger ? deltaWidth : 0);
    float dy = ty + lytDate->LayoutBounds.Height;
    ds->DrawTextLayout(lytDate, dx, dy, Colors::Black);
}
