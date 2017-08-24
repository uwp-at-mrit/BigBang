#include "workspace.h"

using namespace Win2D::StartsWithPanel;
using namespace Win2D::Xaml;

using namespace Windows::UI::Xaml;
using namespace Windows::UI::Xaml::Controls;

WorkSpace::WorkSpace() : StackPanel() {
    Windows::UI::ViewManagement::ApplicationView::GetForCurrentView()->Title = "Monitor";
    Orientation = ::Orientation::Vertical;
    VerticalAlignment = ::VerticalAlignment::Top;
    Margin = ThicknessHelper::FromUniformLength(8.0);
    SizeChanged += ref new SizeChangedEventHandler(this, &WorkSpace::Reflow);
}

void WorkSpace::InitializeComponent() {
    Thickness zero = ThicknessHelper::FromUniformLength(0);
    auto titleBar = XAML::MakeStackPanel(this, ::Orientation::Horizontal, zero, zero);
    switchBar = XAML::MakeStackPanel(titleBar, ::Orientation::Horizontal, zero, zero);

    numeric = XAML::MakeToggleSwitch(switchBar, "numeric", nullptr, nullptr);
    alert = XAML::MakeToggleSwitch(switchBar, "alert", nullptr, nullptr);
    flash = XAML::MakeToggleSwitch(switchBar, "flash", nullptr, nullptr);

    systemClock = ref new DigitalClock(titleBar);
    monitor = XAML::MakeCanvas(this, "monitor");
}

void WorkSpace::Reflow(Object^ sender, SizeChangedEventArgs^ e) {
    if (e->PreviousSize.Width != e->NewSize.Width) {
        systemClock->ChangeSize(e->NewSize.Width - switchBar->ActualWidth, switchBar->ActualHeight);
    }
}

void WorkSpace::Suspend(Object^ sender, Windows::ApplicationModel::SuspendingEventArgs^ e) {
    // TODO: Save application state and stop any background activity.
    // Do not assume that the application will be terminated or resumed with the contents of memory still intact.
}
