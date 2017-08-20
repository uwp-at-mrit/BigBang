#include "pch.h"
#include "workspace.h"

using namespace Win2D::StartsWithPanel;

using namespace Windows::Foundation;
using namespace Windows::UI::ViewManagement;

WorkSpace::WorkSpace() : StackPanel() {
    ApplicationView::GetForCurrentView()->Title = "Monitor";
    Orientation = ::Orientation::Vertical;
    VerticalAlignment = ::VerticalAlignment::Top;
    Margin = ThicknessHelper::FromUniformLength(8.0);
    SizeChanged += ref new SizeChangedEventHandler(this, &WorkSpace::Reflow);
}

void WorkSpace::InitializeComponent() {
    Thickness zero = ThicknessHelper::FromUniformLength(0);
    auto titleBar = UI::MakeStackPanel(this, ::Orientation::Horizontal, zero, zero);
    monitor = UI::MakeCanvas(this, "monitor");

    switchBar = UI::MakeStackPanel(titleBar, ::Orientation::Horizontal, zero, zero);
    systemClock = UI::MakeDigitalClock(titleBar);

    numeric = UI::MakeToggleSwitch(switchBar, "numeric", nullptr, nullptr);
    alert = UI::MakeToggleSwitch(switchBar, "alert", nullptr, nullptr);
    flash = UI::MakeToggleSwitch(switchBar, "flash", nullptr, nullptr);
}

void WorkSpace::Reflow(Object^ sender, SizeChangedEventArgs^ e) {
    if (e->PreviousSize.Width != e->NewSize.Width) {
        double left = switchBar->Padding.Left;
        double top = switchBar->Padding.Top;
        double bottom = switchBar->Padding.Bottom;
        double right = switchBar->Padding.Right;
        double switchOff = (switchBar->ActualWidth - right) + switchBar->Margin.Left + switchBar->Margin.Right;
        double clockOff = systemClock->ActualWidth - systemClock->Padding.Left;
        double padding = e->NewSize.Width - switchOff - clockOff;

        switchBar->Padding = ThicknessHelper::FromLengths(left, top, padding, bottom);
    }
}

void WorkSpace::Suspend(Object^ sender, SuspendingEventArgs^ e) {
    // TODO: Save application state and stop any background activity.
    // Do not assume that the application will be terminated or resumed with the contents of memory still intact.
}
