#pragma once

namespace WarGrey::Win2DDemo {
    private ref class IPointerListener abstract {
    public:
        virtual bool action(
            Platform::Object^ self,
            float x, float y,
            Windows::UI::Input::PointerPointProperties^ ppps,
            Windows::System::VirtualKeyModifiers vkms,
            Windows::Devices::Input::PointerDeviceType type);

        virtual bool notice(
            Platform::Object^ self,
            float x, float y,
            Windows::UI::Input::PointerPointProperties^ ppps,
            Windows::System::VirtualKeyModifiers vkms,
            Windows::Devices::Input::PointerDeviceType type);
    };
}
