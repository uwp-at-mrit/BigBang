#pragma once

#include "forward.hpp"
#include "object.hpp"

namespace WarGrey::SCADA {
	private class IPlanetDecorator abstract : public WarGrey::SCADA::SharedObject {
    public:
        virtual void draw_before(
            WarGrey::SCADA::IPlanet* master,
            Microsoft::Graphics::Canvas::CanvasDrawingSession^ ds,
            float Width, float Height) {};

        virtual void draw_after(
            WarGrey::SCADA::IPlanet* master,
            Microsoft::Graphics::Canvas::CanvasDrawingSession^ ds,
            float Width, float Height) {};

        virtual void draw_before_snip(
            WarGrey::SCADA::ISnip* snip,
            Microsoft::Graphics::Canvas::CanvasDrawingSession^ ds,
            float x, float y, float width, float height) {};

        virtual void draw_after_snip(
            WarGrey::SCADA::ISnip* snip,
            Microsoft::Graphics::Canvas::CanvasDrawingSession^ ds,
            float x, float y, float width, float height) {};

		virtual void draw_for_selected_snip(
			WarGrey::SCADA::ISnip* snip,
			Microsoft::Graphics::Canvas::CanvasDrawingSession^ ds,
			float x, float y, float width, float height) {};
    };
}
