#pragma once

#include "modbus/client.hpp"
#include "universe.hpp"

namespace WarGrey::SCADA {
    [::Windows::Foundation::Metadata::WebHostHidden]
    public ref class Console sealed : public Windows::UI::Xaml::Controls::StackPanel {
    public:
        Console();
        void initialize_component(Windows::Foundation::Size region);

    public:
        void reflow(float width, float height);
        void suspend(Windows::ApplicationModel::SuspendingOperation^ op);

    private:
        ~Console();

    private:
        WarGrey::SCADA::IModbusClient* client;
        WarGrey::SCADA::Universe* universe;
    };
}
