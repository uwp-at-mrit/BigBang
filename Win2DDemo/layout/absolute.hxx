#pragma once

#include "pasteboard.hxx"

namespace WarGrey::Win2DDemo {
    private class AbsoluteLayout : public WarGrey::Win2DDemo::IPasteboardLayout {
    public:
        void before_insert(Pasteboard^ self, Snip* snip, float x, float y) override;
        void after_insert(Pasteboard^ self, Snip* snip, float x, float y) override;
    };
}
