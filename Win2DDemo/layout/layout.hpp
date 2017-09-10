#pragma once

#include "forward.hxx"

namespace WarGrey::Win2DDemo {
    private class IPasteboardLayout abstract {
    public:
        virtual void on_attach_to(Pasteboard^ master) {};
        virtual ~IPasteboardLayout() noexcept {};

    public:
        virtual bool can_move(Pasteboard^ master, Windows::UI::Xaml::Input::PointerRoutedEventArgs^ e) { return true; };

    public:
        virtual void before_insert(Pasteboard^ master, Snip* snip, float x, float y) {};
        virtual void after_insert(Pasteboard^ master, Snip* snip, float x, float y) {};

    public:
        int refcount = 0;
    };
}
