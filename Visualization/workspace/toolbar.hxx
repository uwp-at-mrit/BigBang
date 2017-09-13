#pragma once

#include "forward.hxx"
#include "workspace/listener.hpp"

namespace WarGrey::SCADA {
    private class ToolbarListener : public WarGrey::SCADA::IPasteboardListener {
    public:
        ToolbarListener(WarGrey::SCADA::Pasteboard^ stage);

    public:
        bool can_select_multiple(Pasteboard^ master) override;

    public:
        void after_select(Pasteboard^ master, Snip* snip) override;

    private:
        WarGrey::SCADA::Pasteboard^ stage;
    };
}
