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

    formatter = ref new DateTimeFormatter("longdate longtime");
    datetime = ref new Calendar();
    entity = UI::MakeGPUCanvas(parent, "SystemClock", onLoad, onDraw);
    timer = UI::MakeGUITimer(0, onTick);
}

void DigitalClock::UpdateTimeStamp() {
    datetime->SetToNow();
    long long l00ns = datetime->Nanosecond / 100;
    long long ms = l00ns / 10000;
    timer->Interval = TimeSpan({ 10000000 - l00ns });
    Timestamp = formatter->Format(datetime->GetDateTime())
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
    if (Timestamp == nullptr) {
        UpdateTimeStamp();
    }
}

void DigitalClock::DrawClock(CanvasControl^ sender, CanvasDrawEventArgs^ e) {
    e->DrawingSession->DrawText(Timestamp, 0, 0, Colors::Black);
}

void DigitalClock::ChangeSize(double width, double height) {
    entity->Width = width;
    entity->Height = height;
}
