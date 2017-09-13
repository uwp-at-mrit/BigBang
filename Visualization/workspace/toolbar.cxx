#include "toolbar.hxx"
#include "pasteboard.hxx"
#include "snip/snip.hpp"

using namespace WarGrey::SCADA;

ToolbarListener::ToolbarListener(Pasteboard^ stage) : stage(stage) {}

bool ToolbarListener::can_select_multiple(Pasteboard^ stage) {
    return false;
}

void ToolbarListener::after_select(Pasteboard^ master, Snip* snip) {
    SnipIcon* icon = (SnipIcon*)snip;
    stage->insert(icon->create_snip(), 0, 0);
}
