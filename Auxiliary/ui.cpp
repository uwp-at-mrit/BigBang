#include "ui.hpp"

using namespace Windows::Foundation;

using namespace Windows::UI;
using namespace Windows::UI::Xaml;
using namespace Windows::UI::Xaml::Controls;
using namespace Microsoft::Graphics::Canvas::UI::Xaml;

template <class T>
static inline T append_child(Panel^ parent, T child, Platform::String^ id) {
    if (id != nullptr)  child->Name = id;
    if (parent != nullptr) parent->Children->Append(child);

    return child;
}

StackPanel^ stack_panel(Panel^ parent, Orientation direction, Thickness margin, Thickness padding) {
    auto child = ref new StackPanel();

    child->Orientation = direction;
    child->Margin = margin;
    child->Padding = padding;

    return append_child<StackPanel^>(parent, child, nullptr);
}

CanvasControl^ gpu_canvas(Panel^ parent, Platform::String^ id, CanvasLoadHandler^ do_load, CanvasDrawHandler^ OnDraw) {
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
