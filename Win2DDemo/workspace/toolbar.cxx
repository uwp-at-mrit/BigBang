#include "toolbar.hxx"
#include "debug.hpp"

using namespace WarGrey::Win2DDemo;

ToolbarListener::ToolbarListener(Pasteboard^ stage) : stage(stage) {}

bool ToolbarListener::can_select_multiple(Pasteboard^ stage) {
    return false;
}

void ToolbarListener::after_select(Pasteboard^ master, Snip* snip) {
    SnipIcon* icon = (SnipIcon*)snip;
    stage->insert(icon->create_snip(), 0, 0);
}
