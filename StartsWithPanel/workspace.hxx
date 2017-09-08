#pragma once

#include "ui.hxx"
#include "digitalclock.hxx"
#include "pasteboard.hxx"

namespace WarGrey::Win2DDemo {
    [::Windows::Foundation::Metadata::WebHostHidden]
    public ref class WorkSpace sealed : public Windows::UI::Xaml::Controls::StackPanel {
    public:
        WorkSpace();
        void initialize_component(Windows::Foundation::Size region);

    public:
        void reflow(float width, float height);
        void suspend(Windows::ApplicationModel::SuspendingOperation^ op);

    private:
        Windows::UI::Xaml::Controls::StackPanel^ switchbar;
        WarGrey::Win2DDemo::DigitalClock^ system_clock;
        WarGrey::Win2DDemo::Pasteboard^ toolbar;
        WarGrey::Win2DDemo::Pasteboard^ stage;
    };
}
