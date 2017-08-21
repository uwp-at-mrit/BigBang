#include "pch.h"
#include "ui.h"

using namespace Win2D::XAML;
using namespace Windows::Foundation;

template <class T>
static T PlaceUIElement(Panel^ parent, T child, String^ id) {
    if (id != nullptr)  child->Name = id;

    UIElementCollection^ children = parent->Children;
    children->InsertAt(children->Size, child);

    return child;
}

static void MakeTextBoxAsLabel(TextBox^ label) {
    label->IsReadOnly = true;
    label->IsTabStop = false;
    label->AllowFocusOnInteraction = false;
    label->Background = nullptr;
    label->BorderBrush = nullptr;
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

DigitalClock^ UI::MakeDigitalClock(Panel^ parent) {
    return PlaceUIElement<DigitalClock^>(parent, ref new DigitalClock(), nullptr);
}

Canvas^ UI::MakeCanvas(Panel^ parent, String^ id) {
    auto child = ref new Canvas();

    return PlaceUIElement<Canvas^>(parent, child, id);
}

DispatcherTimer^ UI::MakeGUITimer(long long ms, EventHandler<Object^>^ OnTick) {
    auto timer = ref new DispatcherTimer();
    long long duration = ms * 1000 * 10;

    timer->Interval = TimeSpan({ duration });
    timer->Start();
    timer->Tick += OnTick;

    return timer;
}

/*************************************************************************************************/
DigitalClock::DigitalClock() {
    Name = "SystemClock";
    VerticalAlignment = ::VerticalAlignment::Center;
    Loaded += ref new RoutedEventHandler(this, &DigitalClock::OnLoadTrim);
    formatter = ref new DateTimeFormatter("longdate longtime");
    now = ref new Calendar();
    MakeTextBoxAsLabel(this);
    timer = UI::MakeGUITimer(0, ref new EventHandler<Object^>(this, &DigitalClock::OnTickUpdate));
    OnTickUpdate(nullptr, nullptr); // timer is used in inside the event handler
}

void DigitalClock::OnTickUpdate(Object^ sender, Object^ e) {
    now->SetToNow();
    long long l00ns = now->Nanosecond / 100;
    long long ms = l00ns / 10000;
    timer->Interval = TimeSpan({ 10000000 - l00ns });
    this->Text = formatter->Format(now->GetDateTime())
        + ((ms < 10) ? ".00" : ((ms < 100) ? ".0" : "."))
        + ms.ToString();
}

void DigitalClock::OnLoadTrim(Object^ sender, RoutedEventArgs^ e) {
    if (Padding.Left >= 0.0) {
        Padding = ThicknessHelper::FromLengths(0.0, Padding.Top, 0.0, Padding.Bottom);
    }
}
