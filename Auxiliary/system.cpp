#include "ui.hpp"

using namespace Windows::Foundation;
using namespace Windows::Graphics::Display;

using namespace Windows::UI;
using namespace Windows::UI::Xaml::Controls;
using namespace Windows::UI::ViewManagement;

static UISettings^ sysUI = nullptr;

static inline Size adjust_size(float Width, float Height, Panel^ workspace) {
    auto margin = workspace->Margin;

    float width = Width - float(margin.Left + margin.Right);
    float height = Height - float(margin.Top + margin.Bottom);
    return Size(width, height);
}

Size system_screen_size() {
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
