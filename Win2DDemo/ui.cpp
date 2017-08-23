#include "pch.h"
#include "ui.h"

using namespace Win2D::XAML;
using namespace Windows::UI;

template <class T>
static T PlaceUIElement(Panel^ parent, T child, String^ id) {
    if (id != nullptr)  child->Name = id;
    parent->Children->Append(child);

    return child;
}

StackPanel^ UI::MakeStackPanel(Panel^ parent, Orientation direction, Thickness margin, Thickness padding) {
    auto child = ref new StackPanel();

    child->Orientation = direction;
    child->Margin = margin;
    child->Padding = padding;

    return PlaceUIElement<StackPanel^>(parent, child, nullptr);
}

ToggleSwitch^ UI::MakeToggleSwitch(Panel^ parent, String^ id, String^ onCaption, String^ offCaption) {
    auto child = ref new ToggleSwitch();

    child->OnContent = (onCaption == nullptr) ? ((id == nullptr) ? "" : id) : onCaption;
    child->OffContent = (offCaption == nullptr) ? child->OnContent : offCaption;
    
    return PlaceUIElement<ToggleSwitch^>(parent, child, id);
}

CanvasControl^ UI::MakeGPUCanvas(Panel^ parent, String^ id, CanvasRCHandler^ OnLoad, CanvasDrawHandler^ OnDraw) {
    auto child = ref new CanvasControl();

    child->CreateResources += OnLoad;
    child->Draw += OnDraw;

    return PlaceUIElement<CanvasControl^>(parent, child, id);
}


Canvas^ UI::MakeCanvas(Panel^ parent, String^ id) {
    auto child = ref new Canvas();

    return PlaceUIElement<Canvas^>(parent, child, id);
}

DispatcherTimer^ UI::MakeGUITimer(long long ms, ObjectHandler^ OnTick) {
    auto timer = ref new DispatcherTimer();
    long long duration = ms * 1000 * 10;

    timer->Interval = TimeSpan({ duration });
    timer->Start();
    timer->Tick += OnTick;

    return timer;
}

/*************************************************************************************************/
DigitalClock::DigitalClock(Panel^ parent) {
    auto onTick = ref new ObjectHandler(this, &DigitalClock::OnTickUpdate);
    auto onLoad = ref new CanvasRCHandler(this, &DigitalClock::LoadTimestamp);
    auto onDraw = ref new CanvasDrawHandler(this, &DigitalClock::DrawClock);

    longdate = ref new DateTimeFormatter("longdate");
    longtime = ref new DateTimeFormatter("longtime");
    datetime = ref new Calendar();
    entity = UI::MakeGPUCanvas(parent, "SystemClock", onLoad, onDraw);
    timer = UI::MakeGUITimer(0, onTick);
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
    if (entity != nullptr) {
        entity->Invalidate();
    }
}

void DigitalClock::LoadTimestamp(CanvasControl^ sender, CanvasCreateResourcesEventArgs^ e) {
    if (timestamp == nullptr) {
        UpdateTimeStamp();
    }

    if (fontInfo == nullptr) {
        fontInfo = ref new CanvasTextFormat();
        fontInfo->WordWrapping = CanvasWordWrapping::NoWrap;
        fontInfo->FontSize = 12;
    }
}

void DigitalClock::DrawClock(CanvasControl^ sender, CanvasDrawEventArgs^ e) {
    CanvasTextLayout^ lytTime = ref new CanvasTextLayout(e->DrawingSession, timestamp, fontInfo, 0.0f, 0.0f);
    CanvasTextLayout^ lytDate = ref new CanvasTextLayout(e->DrawingSession, datestamp, fontInfo, 0.0f, 0.0f);
    float width = (float)entity->Width;
    float height = (float)entity->Height;

    bool isTimestampLonger = (lytTime->LayoutBounds.Width > lytDate->LayoutBounds.Width);
    float deltaWidth = abs(lytDate->LayoutBounds.Width - lytTime->LayoutBounds.Width) / 2.0f;
    
    float tx = width - lytTime->LayoutBounds.Width - (isTimestampLonger ? 0.0f : deltaWidth);
    float ty = (height - lytTime->LayoutBounds.Height - lytDate->LayoutBounds.Height) / 2.0f;
    e->DrawingSession->DrawTextLayout(lytTime, tx, ty, Colors::Black);
    
    float dx = width - lytDate->LayoutBounds.Width - (isTimestampLonger ? deltaWidth : 0);
    float dy = ty + lytDate->LayoutBounds.Height;
    e->DrawingSession->DrawTextLayout(lytDate, dx, dy, Colors::Black);
}

void DigitalClock::ChangeSize(double width, double height) {
    entity->Width = width;
    entity->Height = height;
}
