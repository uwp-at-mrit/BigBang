#pragma once

#include "layout.hpp"

namespace WarGrey::Win2DDemo {
    private class AbsoluteLayout : public WarGrey::Win2DDemo::IPasteboardLayout {
    public:
        AbsoluteLayout(float min_width = 0.0F, float min_height = 0.0F);
        void on_attach_to(Pasteboard^ master) override;

    public:
        void before_insert(Pasteboard^ master, Snip* snip, float x, float y) override;
        void after_insert(Pasteboard^ master, Snip* snip, float x, float y) override;

    private:
        float preferred_min_width;
        float preferred_min_height;
    };
}
