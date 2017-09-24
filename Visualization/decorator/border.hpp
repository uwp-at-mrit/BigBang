#pragma once

#include "decorator/decorator.hpp"

namespace WarGrey::SCADA {
    private class BorderDecorator sealed: public WarGrey::SCADA::IPasteboardDecorator {
    public:
        BorderDecorator(bool draw_outer = true, bool draw_inner = true, bool draw_enclosing = false);

    public:
        void draw_before(WarGrey::SCADA::Pasteboard^ master, Microsoft::Graphics::Canvas::CanvasDrawingSession^ ds,
            float width, float height, Windows::Foundation::Rect& outbox) override;

    private:
        bool draw_outer_border;
        bool draw_inner_border;
        bool draw_enclosing_box;
    };
}
