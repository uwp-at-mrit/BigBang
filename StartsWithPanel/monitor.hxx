#pragma once

#include "ui.hxx"
#include "digitalclock.hxx"
#include "pasteboard.hxx"
#include "listener/pointer.hxx"

namespace Win2D::StartsWithPanel {
    [::Windows::Foundation::Metadata::WebHostHidden]
    public ref class Monitor sealed : public Windows::UI::Xaml::Controls::StackPanel {
    public:
        Monitor();

    public:
        void initialize_component();
        void suspend(Object^ sender, Windows::ApplicationModel::SuspendingEventArgs^ e);
        void reflow(Object^ sender, Windows::UI::Xaml::SizeChangedEventArgs^ e);

    private:
        Windows::UI::Xaml::Controls::StackPanel^ switchbar;
        Win2D::UIElement::DigitalClock^ system_clock;
        Win2D::UIElement::IPasteboard^ toolbar;
        Win2D::UIElement::IPasteboard^ stage;
    };
}
