#pragma once

namespace Win2D::UIElement {
    private enum Interaction { ACTION, NOTICE };

    private ref class IInteractive abstract {
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
