#pragma once

#include "pasteboard.hxx"
#include "listener/pointer.hxx"

namespace Win2DDemo {
    private ref class ToolbarListener sealed : public Win2DDemo::IPointerListener {
    public:
        ToolbarListener(Win2DDemo::IPasteboard^ stage);

    public:
        bool action(
            float x, float y,
            Windows::UI::Input::PointerPointProperties^ ppps,
            Windows::System::VirtualKeyModifiers vkms,
            Windows::Devices::Input::PointerDeviceType type) override;

    private:
        Win2DDemo::IPasteboard^ stage;
    };
}
