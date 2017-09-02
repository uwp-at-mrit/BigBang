#pragma once

#include "ui.hxx"
#include "digitalclock.hxx"
#include "pasteboard.hxx"
#include "listener/pointer.hxx"

namespace WarGrey::Win2DDemo {
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
        WarGrey::Win2DDemo::DigitalClock^ system_clock;
        WarGrey::Win2DDemo::IPasteboard^ toolbar;
        WarGrey::Win2DDemo::IPasteboard^ stage;
    };
}
