#include "pch.h"
#include "ui.h"
#include "workspace.h"

using namespace Win2D::XAML;
using namespace Win2D::StartsWithPanel;

using namespace Windows::Foundation;
using namespace Windows::UI::ViewManagement;
using namespace Windows::ApplicationModel::Resources;

using namespace Windows::System::Diagnostics;

WorkSpace::WorkSpace() : StackPanel() {
    Orientation = Windows::UI::Xaml::Controls::Orientation::Vertical; // Vertical is also the default orientation
    ApplicationView::GetForCurrentView()->Title = "Monitor";
};

void WorkSpace::InitializeComponent() {
    auto switchBar = UI::MakeStackPanel(this, Windows::UI::Xaml::Controls::Orientation::Horizontal);
    
    monitor = UI::MakeCanvas(this, "monitor");
    numeric = UI::MakeToggleSwitch(switchBar, "numeric", nullptr, nullptr);
    alert = UI::MakeToggleSwitch(switchBar, "alert", nullptr, nullptr);
    flash = UI::MakeToggleSwitch(switchBar, "flash", nullptr, nullptr);

    auto clockPanel = UI::MakeStackPanel(switchBar, Windows::UI::Xaml::Controls::Orientation::Horizontal);
    clockPanel->HorizontalAlignment = Windows::UI::Xaml::HorizontalAlignment::Right;
    clockPanel->VerticalAlignment = Windows::UI::Xaml::VerticalAlignment::Center;
    UI::MakeTextClock(clockPanel);
};

void WorkSpace::Suspend(Object^ sender, SuspendingEventArgs^ e) {
    // TODO: Save application state and stop any background activity.
    // Do not assume that the application will be terminated or resumed with the contents of memory still intact.
};
