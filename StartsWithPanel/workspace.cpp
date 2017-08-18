#include "pch.h"
#include "workspace.h"

Win2D::WorkSpace::WorkSpace() : StackPanel() {
    Orientation = Windows::UI::Xaml::Controls::Orientation::Vertical; // Vertical is also the default orientation
};

void Win2D::WorkSpace::InitializeComponent() {
    auto buttonPanel = ref new StackPanel();
    buttonPanel->Orientation = Windows::UI::Xaml::Controls::Orientation::Horizontal;

    this->Children->InsertAt(0, buttonPanel);
};

void Win2D::WorkSpace::Suspend(Object^ sender, SuspendingEventArgs^ e) {
    (void)sender;
    (void)e;

    // TODO: Save application state and stop any background activity.
    // Do not assume that the application will be terminated or resumed with the contents of memory still intact.
};
