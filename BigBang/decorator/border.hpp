#pragma once

#include "decorator/decorator.hpp"

namespace WarGrey::SCADA {
    private class BorderDecorator sealed : public WarGrey::SCADA::IUniverseDecorator {
    public:
        BorderDecorator(bool draw_border = true, bool draw_enclosing = false, bool draw_snip_enclosing = false);

    public:
        void draw_before(
            WarGrey::SCADA::Universe* master,
            Microsoft::Graphics::Canvas::CanvasDrawingSession^ ds,
            float Width, float Height) override;

        void draw_before_snip(
            WarGrey::SCADA::Snip* snip,
            Microsoft::Graphics::Canvas::CanvasDrawingSession^ ds,
            float x, float y, float width, float height) override;

    private:
        bool draw_border;
        bool draw_enclosing_box;
        bool draw_snip_enclosing_box;
    };
}
