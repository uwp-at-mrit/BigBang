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

StackPanel^ UI::MakeStackPanel(Panel^ parent, Orientation direction) {
    auto child = ref new StackPanel();

    child->Orientation = direction;

    return PlaceUIElement<StackPanel^>(parent, child, nullptr);
}

ToggleSwitch^ UI::MakeToggleSwitch(Panel^ parent, String^ id, String^ onCaption, String^ offCaption) {
    auto child = ref new ToggleSwitch();

    child->OnContent = (onCaption == nullptr) ? ((id == nullptr) ? "" : id) : onCaption;
    child->OffContent = (offCaption == nullptr) ? child->OnContent : offCaption;
    
    return PlaceUIElement<ToggleSwitch^>(parent, child, id);
}

TextClock^ UI::MakeTextClock(Panel^ parent) {
    auto child = ref new TextClock();
    
    return PlaceUIElement<TextClock^>(parent, child, nullptr);
}

Canvas^ UI::MakeCanvas(Panel^ parent, String^ id) {
    auto child = ref new Canvas();

    return PlaceUIElement<Canvas^>(parent, child, id);
}

/*************************************************************************************************/
TextClock::TextClock() {
    now = ref new Calendar();
    MakeTextBoxAsLabel(this);
    now->SetToNow();
    if (now->Nanosecond == 0) {
        timer = UI::MakeTimer(1000, ref new EventHandler<Object^>(this, &TextClock::UpdateTime));
    } else {
        long long ms = now->Nanosecond / 10000;
        timer = UI::MakeTimer(1000 - ms, ref new EventHandler<Object^>(this, &TextClock::AdjustTime));
    }
}

void TextClock::AdjustTime(Object^ sender, Object^ e) {
    timer->Interval = TimeSpan({ 1000 * 1000 * 10 });
    timer->Tick += ref new EventHandler<Object^>(this, &TextClock::UpdateTime);

    this->UpdateTime(sender, e);
}

void TextClock::UpdateTime(Object^ sender, Object^ e) {
    now->SetToNow();
    this->Text = now->YearAsPaddedString(4)
        + "-" + now->MonthAsPaddedNumericString(2)
        + "-" + now->DayAsPaddedString(2)
        + " " + now->HourAsPaddedString(2)
        + ":" + now->MinuteAsPaddedString(2)
        + ":" + now->SecondAsPaddedString(2);
}

DispatcherTimer^ UI::MakeTimer(long long ms, EventHandler<Object^>^ OnTick) {
    auto timer = ref new DispatcherTimer();
    long long duration = ms * 1000 * 10;

    timer->Interval = TimeSpan({ duration });
    timer->Start();
    timer->Tick += OnTick;

    return timer;
}
