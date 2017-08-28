#include "workspace.h"
#include "snip.h"

using namespace Win2D::StartsWithPanel;
using namespace Win2D::UIElement;
using namespace Win2D::Sniplet;

using namespace Windows::UI;
using namespace Windows::UI::Xaml;
using namespace Windows::UI::Xaml::Controls;
using namespace Windows::UI::Xaml::Shapes;
using namespace Windows::UI::Xaml::Media;

WorkSpace::WorkSpace() : StackPanel() {
    Windows::UI::ViewManagement::ApplicationView::GetForCurrentView()->Title = "Monitor";
    Orientation = ::Orientation::Vertical;
    VerticalAlignment = ::VerticalAlignment::Top;
    Margin = ThicknessHelper::FromUniformLength(8.0);
    SizeChanged += ref new SizeChangedEventHandler(this, &WorkSpace::Reflow);
}

#include <cstdlib>
void WorkSpace::InitializeComponent() {
    Thickness zero = ThicknessHelper::FromUniformLength(0);
    auto titleBar = stack_panel(this, ::Orientation::Horizontal, zero, zero);
    switchBar = stack_panel(titleBar, ::Orientation::Horizontal, zero, zero);
    systemClock = ref new DigitalClock(titleBar);

    numeric = toggle_switch(switchBar, "numeric", nullptr, nullptr);
    alert = toggle_switch(switchBar, "alert", nullptr, nullptr);
    flash = toggle_switch(switchBar, "flash", nullptr, nullptr);

    auto workarea = stack_panel(this, ::Orientation::Horizontal, zero, zero);
    toolbar = ref new ::Pasteboard(workarea, "stage");
    stage = ref new ::Pasteboard(workarea, "stage");

    for (int i = 0; i < 8; i++) {
        unsigned char r = (unsigned char)(rand() % 255);
        unsigned char g = (unsigned char)(rand() % 255);
        unsigned char b = (unsigned char)(rand() % 255);
        auto brush = ref new SolidColorBrush(Color({255, r, g, b}));
        auto square = ref new Rectangle();
        
        square->Fill = brush;
        square->Width = 32;
        square->Height = 32;
    }
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
