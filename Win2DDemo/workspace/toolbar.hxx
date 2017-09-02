#pragma once

#include "pasteboard.hxx"
#include "listener/pointer.hxx"

namespace WarGrey::Win2DDemo {
    private ref class ToolbarListener sealed : public WarGrey::Win2DDemo::IPointerListener {
    public:
        ToolbarListener(WarGrey::Win2DDemo::IPasteboard^ stage);

    public:
        bool action(
            float x, float y,
            Windows::UI::Input::PointerPointProperties^ ppps,
            Windows::System::VirtualKeyModifiers vkms,
            Windows::Devices::Input::PointerDeviceType type) override;

    private:
        WarGrey::Win2DDemo::IPasteboard^ stage;
    };
}
