#pragma once

#include "forward.hxx"

namespace WarGrey::Win2DDemo {
    private class IPasteboardListener abstract {
    public:
        virtual ~IPasteboardListener() noexcept {};

    public:
        virtual bool can_select_multiple(Pasteboard^ master) { return true; };
        virtual bool can_select(Pasteboard^ master, Snip* snip) { return true; };

    public:
        virtual void before_select(Pasteboard^ master, Snip* snip) {};
        virtual void after_select(Pasteboard^ master, Snip* snip) {};
        virtual void before_deselect(Pasteboard^ master, Snip* snip) {};
        virtual void after_deselect(Pasteboard^ master, Snip* snip) {};

    public:
        int refcount = 0;
    };
}
