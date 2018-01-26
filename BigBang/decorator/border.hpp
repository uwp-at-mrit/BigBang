#pragma once

#include "decorator/decorator.hpp"

namespace WarGrey::SCADA {
    private class BorderDecorator : public virtual WarGrey::SCADA::IUniverseDecorator {
    public:
        BorderDecorator(bool draw_border = true, bool draw_enclosing = false, bool draw_selected_snip_enclosing = true);

    public:
        void draw_before(
            WarGrey::SCADA::IPlanet* master,
            Microsoft::Graphics::Canvas::CanvasDrawingSession^ ds,
            float Width, float Height) override;

        void draw_selected_snip(
            WarGrey::SCADA::ISnip* snip,
            Microsoft::Graphics::Canvas::CanvasDrawingSession^ ds,
            float x, float y, float width, float height) override;

    private:
        bool draw_border;
        bool draw_enclosing_box;
        bool draw_snip_enclosing_box;
    };
}
