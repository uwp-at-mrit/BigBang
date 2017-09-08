#include "toolbar.hxx"

using namespace WarGrey::Win2DDemo;

using namespace Windows::System;
using namespace Windows::UI::Input;
using namespace Windows::Devices::Input;

ToolbarListener::ToolbarListener(Pasteboard^ stage) {
    this->stage = stage;
}

/*
bool ToolbarListener::action(Object^ src, float x, float y, PointerPointProperties^ ppps, VirtualKeyModifiers vkms, PointerDeviceType type) {
    Pasteboard^ self = dynamic_cast<Pasteboard^>(src);
    bool handling = (self != nullptr);
    
    if (handling) {
        SnipIcon* snip = (SnipIcon*)self->find_snip(x, y);
        if (snip != nullptr) {
            stage->insert(snip->create_snip(), x, y);
        }
    }

    return handling;
}
*/
