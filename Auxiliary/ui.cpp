#include "ui.hpp"

using namespace Windows::Foundation;
using namespace Windows::Graphics::Display;

using namespace Windows::UI;
using namespace Windows::UI::Xaml;
using namespace Windows::UI::Xaml::Controls;
using namespace Windows::UI::ViewManagement;
using namespace Microsoft::Graphics::Canvas::UI::Xaml;

static UISettings^ sysUI = nullptr;

template <class T>
static inline T append_child(Panel^ parent, T child, Platform::String^ id) {
    if (id != nullptr)  child->Name = id;
    if (parent != nullptr) parent->Children->Append(child);

    return child;
}

static inline Size adjust_size(float Width, float Height, Panel^ workspace) {
    auto margin = workspace->Margin;

    float width = Width - float(margin.Left + margin.Right);
    float height = Height - float(margin.Top + margin.Bottom);
    return Size(width, height);
}

Size get_screen_size() {
    auto display = DisplayInformation::GetForCurrentView();
    auto scaling = float(display->RawPixelsPerViewPixel);

    return { float(display->ScreenWidthInRawPixels) / scaling, float(display->ScreenHeightInRawPixels) / scaling };
}

Size adjusted_workspace_size(Rect region, Panel^ workspace) {
    return adjust_size(region.Width, region.Height, workspace);
}

Color system_color(UIColorType type) {
    if (sysUI == nullptr) {
        sysUI = ref new UISettings();
    }

    return sysUI->GetColorValue(type);
}

Color system_color(UIElementType type) {
    if (sysUI == nullptr) {
        sysUI = ref new UISettings();
    }

    return sysUI->UIElementColor(type);
}

StackPanel^ stack_panel(Panel^ parent, Orientation direction, Thickness margin, Thickness padding) {
    auto child = ref new StackPanel();

    child->Orientation = direction;
    child->Margin = margin;
    child->Padding = padding;

    return append_child<StackPanel^>(parent, child, nullptr);
}

ToggleSwitch^ toggle_switch(Panel^ parent, Platform::String^ id, Platform::String^ onCaption, Platform::String^ offCaption) {
    auto child = ref new ToggleSwitch();

    child->OnContent = (onCaption == nullptr) ? ((id == nullptr) ? "" : id) : onCaption;
    child->OffContent = (offCaption == nullptr) ? child->OnContent : offCaption;
    
    return append_child<ToggleSwitch^>(parent, child, id);
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
