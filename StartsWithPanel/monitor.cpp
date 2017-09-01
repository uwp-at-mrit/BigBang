#include <cstdlib>

#include "monitor.hpp"
#include "pasteboard.hpp"
#include "snip/textlet.hpp"

using namespace Win2D::StartsWithPanel;
using namespace Win2D::UIElement;
using namespace Win2D::Snip;

using namespace Platform;
using namespace Windows::UI;
using namespace Windows::UI::ViewManagement;

using namespace Windows::UI::Xaml;
using namespace Windows::UI::Xaml::Controls;
using namespace Windows::UI::Xaml::Shapes;
using namespace Windows::UI::Xaml::Media;

Monitor::Monitor() : StackPanel() {
    this->Orientation = ::Orientation::Vertical;
    this->Margin = ThicknessHelper::FromUniformLength(8.0);
    this->SizeChanged += ref new SizeChangedEventHandler(this, &Monitor::reflow);

    ApplicationView::GetForCurrentView()->Title = "WorkSpace";
}

void Monitor::initialize_component() {
    Thickness zero = ThicknessHelper::FromUniformLength(0);
    Thickness four = ThicknessHelper::FromUniformLength(4.0);
    auto titleBar = stack_panel(this, ::Orientation::Horizontal, zero, zero);
    this->switchbar = stack_panel(titleBar, ::Orientation::Horizontal, zero, zero);
    this->system_clock = ref new DigitalClock(titleBar);

    auto numeric = toggle_switch(switchbar, "numeric", nullptr, nullptr);
    auto alert = toggle_switch(switchbar, "alert", nullptr, nullptr);
    auto flash = toggle_switch(switchbar, "flash", nullptr, nullptr);

    auto workarea = stack_panel(this, ::Orientation::Horizontal, zero, zero);
    this->toolbar = ref new VPasteboard(workarea, "toolbar", float(four.Top + four.Bottom), four);
    this->stage = ref new Pasteboard(workarea, "stage", four);

    for (int i = 0; i < 8; i++) {
        auto r = (unsigned char)(rand() % 255);
        auto g = (unsigned char)(rand() % 255);
        auto b = (unsigned char)(rand() % 255);

        toolbar->insert(make_textlet_icon(32.0f, r, g, b));
    }

    this->stage->insert(new Textlet("Hi, there! I have builtin drawing region support, so this word is truncated!"));
}

void Monitor::reflow(Object^ sender, SizeChangedEventArgs^ e) {
    // TODO: This Event is not fired when height is shrunken.
    /**
     * TODO: Why I have to handle this?
     *  Are there any problems with CanvasControl whoes ActualWidths happen to be 0
     *   while their ActualHeights are stretched properly?
     */
    bool width_changed = (e->PreviousSize.Width != e->NewSize.Width);
    bool height_changed = (e->PreviousSize.Height != e->NewSize.Height);

    if (width_changed || height_changed) {
        double widthOff = toolbar->canvas->MinWidth;

        this->toolbar->resize(e->NewSize.Width, e->NewSize.Height - switchbar->ActualHeight);
        this->stage->resize(e->NewSize.Width - widthOff, e->NewSize.Height - switchbar->ActualHeight);

        if (width_changed) {
            this->system_clock->resize(e->NewSize.Width - switchbar->ActualWidth, switchbar->ActualHeight);
        }
    }
}

void Monitor::suspend(Object^ sender, Windows::ApplicationModel::SuspendingEventArgs^ e) {
    // TODO: Save application state and stop any background activity.
    // Do not assume that the application will be terminated or resumed with the contents of memory still intact.
}
