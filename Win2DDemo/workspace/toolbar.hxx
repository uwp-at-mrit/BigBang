#pragma once

#include "input.hxx"
#include "pasteboard.hxx"

namespace Win2D::Workspace {
    private ref class ToolbarListener sealed : public Win2D::UIElement::IInteractive {
    public:
        ToolbarListener(Win2D::UIElement::IPasteboard^ stage);

    public:
        bool action(
            float x, float y,
            Windows::UI::Input::PointerPointProperties^ ppps,
            Windows::System::VirtualKeyModifiers vkms,
            Windows::Devices::Input::PointerDeviceType type) override;

    private:
        Win2D::UIElement::IPasteboard^ stage;
    };
}
