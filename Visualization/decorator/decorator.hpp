#pragma once

#include "forward.hpp"

namespace WarGrey::SCADA {
    private class IPasteboardDecorator abstract {
    public:
        virtual ~IPasteboardDecorator() noexcept {};

    public:
        virtual void draw_before(WarGrey::SCADA::Pasteboard^ master, Microsoft::Graphics::Canvas::CanvasDrawingSession^ ds,
            float width, float height, Windows::Foundation::Rect& outbox) {};

        virtual void draw_after(WarGrey::SCADA::Pasteboard^ master, Microsoft::Graphics::Canvas::CanvasDrawingSession^ ds,
            float width, float height, Windows::Foundation::Rect& outbox) {};

    public:
        int refcount = 0;
    };
}
