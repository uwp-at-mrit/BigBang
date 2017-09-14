#pragma once

#include "ui.hxx"
#include "digitalclock.hxx"
#include "pasteboard.hxx"
#include "network.hxx"

namespace WarGrey::SCADA {
    [::Windows::Foundation::Metadata::WebHostHidden]
    public ref class WorkSpace sealed : public Windows::UI::Xaml::Controls::StackPanel {
    public:
        WorkSpace();
        void initialize_component(Windows::Foundation::Size region);

    public:
        void reflow(float width, float height);
        void suspend(Windows::ApplicationModel::SuspendingOperation^ op);

    private:
        WarGrey::SCADA::DigitalClock^ system_clock;
        WarGrey::SCADA::Pasteboard^ toolbar;
        WarGrey::SCADA::Pasteboard^ stage;

        WarGrey::SCADA::TCPListener^ listener;
    };
}
