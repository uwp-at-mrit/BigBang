#pragma once

#include "forward.hpp"

namespace WarGrey::SCADA {
    private class IUniverseDecorator abstract {
    public:
        virtual ~IUniverseDecorator() noexcept {};

    public:
        virtual void draw_before(
            WarGrey::SCADA::Universe* master,
            Microsoft::Graphics::Canvas::CanvasDrawingSession^ ds,
            float Width, float Height) {};

        virtual void draw_after(
            WarGrey::SCADA::Universe* master,
            Microsoft::Graphics::Canvas::CanvasDrawingSession^ ds,
            float Width, float Height) {};

    public:
        int refcount = 0;
    };
}
