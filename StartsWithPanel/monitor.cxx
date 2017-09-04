#include <cstdlib>
#include <algorithm>

#include "monitor.hxx"
#include "pasteboard.hxx"
#include "snip/textlet.hpp"
#include "workspace/toolbar.hxx"
#include "layout/orientation.hxx"

using namespace std;
using namespace Platform;
using namespace WarGrey::Win2DDemo;

using namespace Windows::UI;
using namespace Windows::UI::Core;
using namespace Windows::UI::ViewManagement;

using namespace Windows::UI::Xaml;
using namespace Windows::UI::Xaml::Controls;
using namespace Windows::UI::Xaml::Media;

Monitor::Monitor() : StackPanel() {
    this->Orientation = ::Orientation::Vertical;
    this->Margin = ThicknessHelper::FromUniformLength(8.0);
    this->SizeChanged += ref new SizeChangedEventHandler(this, &Monitor::stretch_workarea);

    ApplicationView::GetForCurrentView()->Title = "WorkSpace";
}

void Monitor::initialize_component() {
    Thickness zero = ThicknessHelper::FromUniformLength(0.0);
    Thickness four = ThicknessHelper::FromUniformLength(4.0);
    auto titleBar = stack_panel(this, ::Orientation::Horizontal, zero, zero);
    this->switchbar = stack_panel(titleBar, ::Orientation::Horizontal, zero, zero);
    this->system_clock = ref new DigitalClock(titleBar);

    auto numeric = toggle_switch(switchbar, "numeric", nullptr, nullptr);
    auto alert = toggle_switch(switchbar, "alert", nullptr, nullptr);
    auto flash = toggle_switch(switchbar, "flash", nullptr, nullptr);

    auto workarea = stack_panel(this, ::Orientation::Horizontal, zero, zero);
    this->toolbar = ref new Pasteboard(workarea, "toolbar", ref new VerticalLayout(float(four.Top + four.Bottom)));
    this->stage = ref new Pasteboard(workarea, "stage", ref new VerticalLayout(0.0F));

    this->toolbar->set_pointer_lisener(ref new ToolbarListener(this->stage));
    this->toolbar->begin_edit_sequence();
    for (int i = 0; i < 4; i++) {
        auto r = (unsigned char)(rand() % 255);
        auto g = (unsigned char)(rand() % 255);
        auto b = (unsigned char)(rand() % 255);

        toolbar->insert(make_textlet_icon(64.0F, r, g, b));
    }
    this->toolbar->end_edit_sequence();

    this->stage->insert(new Textlet("Hi, there! I have builtin drawing region supportted, so these words are truncated!"));
 }

void Monitor::stretch_workarea(Object^ sender, SizeChangedEventArgs^ e) {
    bool width_changed = (e->PreviousSize.Width != e->NewSize.Width);
    bool height_changed = (e->PreviousSize.Height != e->NewSize.Height);

    if (width_changed || height_changed) {
        this->toolbar->canvas_height = e->NewSize.Height - float(switchbar->ActualHeight);
        this->stage->canvas_width = e->NewSize.Width - this->toolbar->actual_width;

        if (width_changed) {
            this->system_clock->resize(e->NewSize.Width - switchbar->ActualWidth, switchbar->ActualHeight);
        }
    }
}

void Monitor::suspend(Object^ sender, Windows::ApplicationModel::SuspendingEventArgs^ e) {
    // TODO: Save application state and stop any background activity.
    // Do not assume that the application will be terminated or resumed with the contents of memory still intact.
}
