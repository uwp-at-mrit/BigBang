#pragma once

namespace WarGrey::Win2DDemo {
    private ref class IPointerListener abstract {
    public:
        virtual bool action(
            float x, float y,
            Windows::UI::Input::PointerPointProperties^ ppps,
            Windows::System::VirtualKeyModifiers vkms,
            Windows::Devices::Input::PointerDeviceType type);

        virtual bool notice(
            float x, float y,
            Windows::UI::Input::PointerPointProperties^ ppps,
            Windows::System::VirtualKeyModifiers vkms,
            Windows::Devices::Input::PointerDeviceType type);
    };
}
