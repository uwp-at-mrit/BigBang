#pragma once

#include "forward.hxx"
#include "workspace/listener.hpp"

namespace WarGrey::WinACS {
    private class ToolbarListener : public WarGrey::WinACS::IPasteboardListener {
    public:
        ToolbarListener(WarGrey::WinACS::Pasteboard^ stage);

    public:
        bool can_select_multiple(Pasteboard^ master) override;

    public:
        void after_select(Pasteboard^ master, Snip* snip) override;

    private:
        WarGrey::WinACS::Pasteboard^ stage;
    };
}
