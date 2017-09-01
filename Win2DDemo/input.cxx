#include "input.hxx"

using namespace Win2D::UIElement;

using namespace Windows::System;
using namespace Windows::UI::Input;
using namespace Windows::Devices::Input;

bool IInteractive::action(float x, float y, PointerPointProperties^ ppps, VirtualKeyModifiers vkms, PointerDeviceType type) {
    return false;
}

bool IInteractive::notice(float x, float y, PointerPointProperties^ ppps, VirtualKeyModifiers vkms, PointerDeviceType type) {
    return false;
}
