#pragma once

#include "pasteboard.hxx"

namespace WarGrey::Win2DDemo {
    private class ToolbarListener : public WarGrey::Win2DDemo::IPasteboardListener {
    public:
        ToolbarListener(WarGrey::Win2DDemo::Pasteboard^ stage);

    private:
        WarGrey::Win2DDemo::Pasteboard^ stage;
    };
}
