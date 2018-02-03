#pragma once

#include "decorator/border.hpp"
#include "planet.hpp"
#include "system.hpp"
#include "paint.hpp"

using namespace WarGrey::SCADA;

using namespace Microsoft::Graphics::Canvas;

BorderDecorator::BorderDecorator(bool draw_border, bool draw_enclosing, bool draw_snip_enclosing) {
    this->draw_border = draw_border;
    this->draw_enclosing_box = draw_enclosing;
    this->draw_snip_enclosing_box = draw_snip_enclosing;
}

void BorderDecorator::draw_before(IPlanet* master, CanvasDrawingSession^ ds, float Width, float Height) {
    if (this->draw_enclosing_box) {
        float x, y, width, height;

        master->fill_snips_bounds(&x, &y, &width, &height);
        ds->DrawRectangle(x, y, width, height, system_graytext_brush(), 1.0F);
    }

    if (this->draw_border) {
        ds->DrawRectangle(0.0F, 0.0F, Width, Height, system_accentdark1_brush());
    }
}

void BorderDecorator::draw_for_selected_snip(ISnip* self, CanvasDrawingSession^ ds, float x, float y, float width, float height) {
    if (this->draw_snip_enclosing_box) {
        ds->DrawRectangle(x, y, width, height, system_highlight_brush(), 1.0F);
    }
}
