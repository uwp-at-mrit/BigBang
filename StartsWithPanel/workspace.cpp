#include <cstdlib>

#include "workspace.h"
#include "pasteboard.h"
#include "snip/Textlet.h"

using namespace Win2D::StartsWithPanel;
using namespace Win2D::UIElement;
using namespace Win2D::Sniplet;

using namespace Windows::UI;
using namespace Windows::UI::ViewManagement;

using namespace Windows::UI::Xaml;
using namespace Windows::UI::Xaml::Controls;
using namespace Windows::UI::Xaml::Shapes;
using namespace Windows::UI::Xaml::Media;

WorkSpace::WorkSpace() : StackPanel() {
    Orientation = ::Orientation::Vertical;
    Margin = ThicknessHelper::FromUniformLength(8.0);

    ApplicationView::GetForCurrentView()->Title = "WorkSpace";
    SizeChanged += ref new SizeChangedEventHandler(this, &WorkSpace::Reflow);
}

void WorkSpace::InitializeComponent() {
    Thickness zero = ThicknessHelper::FromUniformLength(0);
    auto titleBar = stack_panel(this, ::Orientation::Horizontal, zero, zero);
    switchBar = stack_panel(titleBar, ::Orientation::Horizontal, zero, zero);
    systemClock = ref new DigitalClock(titleBar);

    auto numeric = toggle_switch(switchBar, "numeric", nullptr, nullptr);
    auto alert = toggle_switch(switchBar, "alert", nullptr, nullptr);
    auto flash = toggle_switch(switchBar, "flash", nullptr, nullptr);

    auto workarea = stack_panel(this, ::Orientation::Horizontal, zero, zero);
    toolbar = ref new ::Pasteboard(workarea, "toolbar");
    stage = ref new VerticalPasteboard(workarea, "stage", 8);

    for (int i = 0; i < 8; i++) {
        unsigned char r = (unsigned char)(rand() % 255);
        unsigned char g = (unsigned char)(rand() % 255);
        unsigned char b = (unsigned char)(rand() % 255);
        auto brush = ref new SolidColorBrush(Color({255, r, g, b}));
        auto square = ref new Rectangle();
        
        square->Fill = brush;
        square->Width = 32;
        square->Height = 32;

        stage->Insert(new Textlet("Hello, Snip" + i.ToString()));
    }
}

void WorkSpace::Reflow(Object^ sender, SizeChangedEventArgs^ e) {
    // TODO: This Event is not fired when height become less.
    bool widthChanged = (e->PreviousSize.Width != e->NewSize.Width);
    bool heightChanged = (e->PreviousSize.Height != e->NewSize.Height);

    if (widthChanged || heightChanged) {
        stage->ChangeSize(e->NewSize.Width, e->NewSize.Height - switchBar->ActualHeight);

        if (widthChanged) {
            systemClock->ChangeSize(e->NewSize.Width - switchBar->ActualWidth, switchBar->ActualHeight);
        }
    }
}

void WorkSpace::Suspend(Object^ sender, Windows::ApplicationModel::SuspendingEventArgs^ e) {
    // TODO: Save application state and stop any background activity.
    // Do not assume that the application will be terminated or resumed with the contents of memory still intact.
}
