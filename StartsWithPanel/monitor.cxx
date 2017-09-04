#include <cstdlib>
#include <algorithm>

#include "monitor.hxx"
#include "pasteboard.hxx"
#include "snip/textlet.hpp"
#include "workspace/toolbar.hxx"

using namespace std;
using namespace Platform;
using namespace WarGrey::Win2DDemo;

using namespace Windows::UI;
using namespace Windows::UI::ViewManagement;

using namespace Windows::UI::Xaml;
using namespace Windows::UI::Xaml::Controls;
using namespace Windows::UI::Xaml::Media;

Monitor::Monitor() : StackPanel() {
    this->Orientation = ::Orientation::Vertical;
    this->Margin = ThicknessHelper::FromUniformLength(8.0);
    this->SizeChanged += ref new SizeChangedEventHandler(this, &Monitor::reflow);

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
    this->toolbar = ref new VPasteboard(workarea, "toolbar", float(four.Top + four.Bottom), four);
    this->stage = ref new VPasteboard(workarea, "stage", 0.0F, four);

    this->toolbar->set_pointer_lisener(ref new ToolbarListener(this->stage));

    for (int i = 0; i < 4; i++) {
        auto r = (unsigned char)(rand() % 255);
        auto g = (unsigned char)(rand() % 255);
        auto b = (unsigned char)(rand() % 255);

        toolbar->insert(make_textlet_icon(64.0F, r, g, b));
    }

    this->stage->insert(new Textlet("Hi, there! I have builtin drawing region supportted, so these words are truncated!"));
    this->stage->insert(new Textlet("Universay Windows Platform seems to have bugs!!"));
}

void Monitor::reflow(Object^ sender, SizeChangedEventArgs^ e) {
    // TODO: This Event is not fired for shrinking height.
    // TODO: Why I have to deal with this mannually?
    bool width_changed = (e->PreviousSize.Width != e->NewSize.Width);
    bool height_changed = (e->PreviousSize.Height != e->NewSize.Height);

    if (width_changed || height_changed) {
        float toolbar_width;

        this->toolbar->fill_snips_extent(nullptr, nullptr, &toolbar_width, nullptr);
        this->toolbar->layer_width = toolbar_width;
        this->toolbar->min_canvas_height = 400.0F;  //TODO: why e->NewSize.Height crashes the application
        this->stage->canvas_width = e->NewSize.Width - this->toolbar->min_canvas_width;

        this->stage->insert(new Textlet(L"Stage Size: (%f, %f)", e->NewSize.Width, e->NewSize.Height));

        if (width_changed) {
            this->system_clock->resize(e->NewSize.Width - switchbar->ActualWidth, switchbar->ActualHeight);
        }
    }
}

void Monitor::suspend(Object^ sender, Windows::ApplicationModel::SuspendingEventArgs^ e) {
    // TODO: Save application state and stop any background activity.
    // Do not assume that the application will be terminated or resumed with the contents of memory still intact.
}
