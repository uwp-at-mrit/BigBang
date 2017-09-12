#pragma once

#include "ui.hxx"
#include "digitalclock.hxx"
#include "pasteboard.hxx"

namespace WarGrey::WinACS {
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
        WarGrey::WinACS::DigitalClock^ system_clock;
        WarGrey::WinACS::Pasteboard^ toolbar;
        WarGrey::WinACS::Pasteboard^ stage;
    };
}
