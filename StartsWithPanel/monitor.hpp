#pragma once

#include "ui.hpp"
#include "digitalclock.hpp"
#include "pasteboard.hpp"

namespace Win2D::StartsWithPanel {
    /// <summary> An empty page that can be used on its own or navigated to within a Frame. </summary>
    [::Windows::Foundation::Metadata::WebHostHidden]
    ref class Monitor sealed : public Windows::UI::Xaml::Controls::StackPanel {
    public:
        Monitor();

    public:
        void InitializeComponent();
        void Suspend(Object^ sender, Windows::ApplicationModel::SuspendingEventArgs^ e);
        void Reflow(Object^ sender, Windows::UI::Xaml::SizeChangedEventArgs^ e);

    private:
        Windows::UI::Xaml::Controls::StackPanel^ switchBar;
        Win2D::UIElement::DigitalClock^ systemClock;
        Win2D::UIElement::IPasteboard^ toolbar;
        Win2D::UIElement::IPasteboard^ stage;
    };
}
