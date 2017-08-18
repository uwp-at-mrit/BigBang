#include "pch.h"
#include "workspace.h"

Win2D::WorkSpace::WorkSpace() {
    Orientation = Windows::UI::Xaml::Controls::Orientation::Vertical; // Vertical is also the default orientation
};

void Win2D::WorkSpace::InitializeComponent() {

};

void Win2D::WorkSpace::Suspend(Object^ sender, SuspendingEventArgs^ e) {
    (void)sender;
    (void)e;

    // TODO: Save application state and stop any background activity.
    // Do not assume that the application will be terminated or resumed with the contents of memory still intact.
};
