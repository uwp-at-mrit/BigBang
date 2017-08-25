#pragma once

#include "ui.h"
#include "digitalclock.h"

namespace Win2D::StartsWithPanel {
    /// <summary> An empty page that can be used on its own or navigated to within a Frame. </summary>
    [::Windows::Foundation::Metadata::WebHostHidden]
    ref class WorkSpace sealed : public Windows::UI::Xaml::Controls::StackPanel {
    public:
        WorkSpace();

    public:
        void InitializeComponent();
        void Suspend(Object^ sender, Windows::ApplicationModel::SuspendingEventArgs^ e);

    private:
        void Reflow(Object^ sender, Windows::UI::Xaml::SizeChangedEventArgs^ e);

    private:
        Windows::UI::Xaml::Controls::StackPanel^ switchBar;
        Win2D::UIElement::DigitalClock^ systemClock;

        Windows::UI::Xaml::Controls::ToggleSwitch^ numeric;
        Windows::UI::Xaml::Controls::ToggleSwitch^ alert;
        Windows::UI::Xaml::Controls::ToggleSwitch^ flash;
        Win2D::UIElement::Pasteboard^ monitor;
    };
}
