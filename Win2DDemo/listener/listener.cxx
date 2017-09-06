#include "pointer.hxx"

using namespace Platform;
using namespace WarGrey::Win2DDemo;

using namespace Windows::System;
using namespace Windows::UI::Input;
using namespace Windows::Devices::Input;

bool IPointerListener::action(Object^ self, float x, float y, PointerPointProperties^ ppps, VirtualKeyModifiers vkms, PointerDeviceType type) {
    return false;
}

bool IPointerListener::notice(Object^ self, float x, float y, PointerPointProperties^ ppps, VirtualKeyModifiers vkms, PointerDeviceType type) {
    return false;
}
