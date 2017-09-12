#include <cstdlib>
#include <algorithm>

#include "workspace.hxx"
#include "pasteboard.hxx"
#include "snip/textlet.hpp"
#include "snip/alarmlet.hpp"
#include "workspace/toolbar.hxx"
#include "layout/orientation.hpp"
#include "layout/absolute.hpp"

using namespace WarGrey::Win2DDemo;

using namespace Windows::Foundation;
using namespace Windows::ApplicationModel;

using namespace Windows::UI;
using namespace Windows::UI::Core;
using namespace Windows::UI::Xaml;
using namespace Windows::UI::Xaml::Controls;
using namespace Windows::UI::Xaml::Media;
using namespace Windows::UI::ViewManagement;

WorkSpace::WorkSpace() : StackPanel() {
    this->Orientation = ::Orientation::Vertical;
    this->Margin = ThicknessHelper::FromUniformLength(8.0);
}

void WorkSpace::initialize_component(Size region) {
    Thickness zero = ThicknessHelper::FromUniformLength(0.0);
    Thickness four = ThicknessHelper::FromUniformLength(4.0);
    auto titleBar = stack_panel(this, ::Orientation::Horizontal, zero, zero);
    this->switchbar = stack_panel(titleBar, ::Orientation::Horizontal, zero, zero);
    this->system_clock = ref new DigitalClock(titleBar);

    auto numeric = toggle_switch(switchbar, "numeric", nullptr, nullptr);
    auto alert = toggle_switch(switchbar, "alert", nullptr, nullptr);
    auto flash = toggle_switch(switchbar, "flash", nullptr, nullptr);

    auto workarea = stack_panel(this, ::Orientation::Horizontal, zero, zero);
    this->toolbar = ref new Pasteboard(workarea, "toolbar", new VerticalLayout(float(four.Top + four.Bottom)));
    this->stage = ref new Pasteboard(workarea, "stage", new AbsoluteLayout(400.0F, 300.0F));
    
    this->toolbar->set_pointer_listener(new ToolbarListener(this->stage));
    auto sysUI = ref new UISettings();
    float icon_size = 64.0F;
    Color icon_color = sysUI->UIElementColor(UIElementType::ActiveCaption);
    this->toolbar->begin_edit_sequence();
    toolbar->insert(make_alarmlet_icon(icon_size, icon_color));
    toolbar->insert(make_textlet_icon(icon_size, icon_color));
    this->toolbar->end_edit_sequence();

    this->reflow(region.Width, region.Height);
}

void WorkSpace::reflow(float width, float height) {
    this->toolbar->canvas_height = height - float(switchbar->ActualHeight);
    this->stage->canvas_width = width - this->toolbar->actual_width;

    this->system_clock->resize(width - switchbar->ActualWidth, switchbar->ActualHeight);
}

void WorkSpace::suspend(SuspendingOperation^ op) {
    // TODO: Save application state and stop any background activity.
    // Do not assume that the application will be terminated or resumed with the contents of memory still intact.
}
