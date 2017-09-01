#include "ui.hpp"

using namespace Platform;
using namespace Windows::Foundation;

using namespace Windows::UI;
using namespace Windows::UI::Xaml;
using namespace Windows::UI::Xaml::Controls;
using namespace Microsoft::Graphics::Canvas::UI::Xaml;

template <class T>
static T append_child(Panel^ parent, T child, String^ id) {
    if (id != nullptr)  child->Name = id;
    parent->Children->Append(child);

    return child;
}

StackPanel^ stack_panel(Panel^ parent, Orientation direction, Thickness margin, Thickness padding) {
    auto child = ref new StackPanel();

    child->Orientation = direction;
    child->Margin = margin;
    child->Padding = padding;

    return append_child<StackPanel^>(parent, child, nullptr);
}

ToggleSwitch^ toggle_switch(Panel^ parent, String^ id, String^ onCaption, String^ offCaption) {
    auto child = ref new ToggleSwitch();

    child->OnContent = (onCaption == nullptr) ? ((id == nullptr) ? "" : id) : onCaption;
    child->OffContent = (offCaption == nullptr) ? child->OnContent : offCaption;
    
    return append_child<ToggleSwitch^>(parent, child, id);
}

CanvasControl^ gpu_canvas(Panel^ parent, String^ id, CanvasLoadHandler^ do_load, CanvasDrawHandler^ OnDraw) {
    auto child = ref new CanvasControl();

    child->CreateResources += do_load;
    child->Draw += OnDraw;

    return append_child<CanvasControl^>(parent, child, id);
}

DispatcherTimer^ gui_timer(long long ms, ObjectHandler^ OnTick) {
    auto timer = ref new DispatcherTimer();
    long long duration = ms * 1000 * 10;

    timer->Interval = TimeSpan({ duration });
    timer->Start();
    timer->Tick += OnTick;

    return timer;
}
