#pragma once

#include "forward.hpp"
#include "object.hpp"

namespace WarGrey::SCADA {
	private class IUniverseDecorator abstract : public WarGrey::SCADA::SharedObject {
    public:
        virtual void draw_before(
            WarGrey::SCADA::IUniverse* master,
            Microsoft::Graphics::Canvas::CanvasDrawingSession^ ds,
            float Width, float Height) {};

        virtual void draw_after(
            WarGrey::SCADA::IUniverse* master,
            Microsoft::Graphics::Canvas::CanvasDrawingSession^ ds,
            float Width, float Height) {};

        virtual void draw_before_snip(
            WarGrey::SCADA::Snip* snip,
            Microsoft::Graphics::Canvas::CanvasDrawingSession^ ds,
            float x, float y, float width, float height) {};

        virtual void draw_after_snip(
            WarGrey::SCADA::Snip* snip,
            Microsoft::Graphics::Canvas::CanvasDrawingSession^ ds,
            float x, float y, float width, float height) {};
    };
}
