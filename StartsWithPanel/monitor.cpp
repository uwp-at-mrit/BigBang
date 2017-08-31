#include <cstdlib>

#include "monitor.h"
#include "pasteboard.h"
#include "snip/Textlet.h"

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
    this->SizeChanged += ref new SizeChangedEventHandler(this, &Monitor::Reflow);

    ApplicationView::GetForCurrentView()->Title = "WorkSpace";
}

void Monitor::InitializeComponent() {
    Thickness zero = ThicknessHelper::FromUniformLength(0);
    Thickness four = ThicknessHelper::FromUniformLength(4.0);
    auto titleBar = stack_panel(this, ::Orientation::Horizontal, zero, zero);
    this->switchBar = stack_panel(titleBar, ::Orientation::Horizontal, zero, zero);
    this->systemClock = ref new DigitalClock(titleBar);

    auto numeric = toggle_switch(switchBar, "numeric", nullptr, nullptr);
    auto alert = toggle_switch(switchBar, "alert", nullptr, nullptr);
    auto flash = toggle_switch(switchBar, "flash", nullptr, nullptr);

    auto workarea = stack_panel(this, ::Orientation::Horizontal, zero, zero);
    this->toolbar = ref new VPasteboard(workarea, "toolbar", four.Top + four.Bottom, four);
    this->stage = ref new Pasteboard(workarea, "stage", four);

    for (int i = 0; i < 8; i++) {
        auto r = (unsigned char)(rand() % 255);
        auto g = (unsigned char)(rand() % 255);
        auto b = (unsigned char)(rand() % 255);

        toolbar->Insert(Textlet::CreateSnipIcon(32.0f, r, g, b));
    }

    this->stage->Insert(new Textlet("Hi, there! I have builtin drawing region support, so this word is truncated!"));
}

void Monitor::Reflow(Object^ sender, SizeChangedEventArgs^ e) {
    // TODO: This Event is not fired when height is shrunken.
    /**
     * TODO: Why I have to handle this?
     *  Are there any problems with CanvasControl whoes ActualWidths happen to be 0
     *   while their ActualHeights are stretched properly?
     */
    bool widthChanged = (e->PreviousSize.Width != e->NewSize.Width);
    bool heightChanged = (e->PreviousSize.Height != e->NewSize.Height);

    if (widthChanged || heightChanged) {
        double widthOff = toolbar->Control->MinWidth;

        this->toolbar->ChangeSize(e->NewSize.Width, e->NewSize.Height - switchBar->ActualHeight);
        this->stage->ChangeSize(e->NewSize.Width - widthOff, e->NewSize.Height - switchBar->ActualHeight);

        if (widthChanged) {
            this->systemClock->ChangeSize(e->NewSize.Width - switchBar->ActualWidth, switchBar->ActualHeight);
        }
    }
}

void Monitor::Suspend(Object^ sender, Windows::ApplicationModel::SuspendingEventArgs^ e) {
    // TODO: Save application state and stop any background activity.
    // Do not assume that the application will be terminated or resumed with the contents of memory still intact.
}
