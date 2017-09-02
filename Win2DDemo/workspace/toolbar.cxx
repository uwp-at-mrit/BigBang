#include "toolbar.hxx"
#include "debug.hpp"

using namespace Win2DDemo;

using namespace Windows::System;
using namespace Windows::UI::Input;
using namespace Windows::Devices::Input;

ToolbarListener::ToolbarListener(IPasteboard^ stage) {
    this->stage = stage;
}

bool ToolbarListener::action(float x, float y, PointerPointProperties^ ppps, VirtualKeyModifiers vkms, PointerDeviceType type) {
    trace(L"[%f, %f]", x, y);
    return true;
}
