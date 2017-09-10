#pragma once

#include "forward.hxx"
#include "workspace/listener.hpp"

namespace WarGrey::Win2DDemo {
    private class ToolbarListener : public WarGrey::Win2DDemo::IPasteboardListener {
    public:
        ToolbarListener(WarGrey::Win2DDemo::Pasteboard^ stage);

    public:
        bool can_select_multiple(Pasteboard^ master) override;

    public:
        void after_select(Pasteboard^ master, Snip* snip) override;

    private:
        WarGrey::Win2DDemo::Pasteboard^ stage;
    };
}
