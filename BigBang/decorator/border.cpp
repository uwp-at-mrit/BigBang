#pragma once

#include "decorator/border.hpp"

#include "planet.hpp"
#include "paint.hpp"
#include "brushes.hxx"

using namespace WarGrey::SCADA;

using namespace Microsoft::Graphics::Canvas;

BorderDecorator::BorderDecorator(bool draw_border, bool draw_enclosing) {
    this->draw_border = draw_border;
    this->draw_enclosing_box = draw_enclosing;
}

void BorderDecorator::draw_before(IPlanet* master, CanvasDrawingSession^ ds, float Width, float Height) {
    if (this->draw_enclosing_box) {
        float x, y, width, height;

        master->fill_graphlets_bounds(&x, &y, &width, &height);
        ds->DrawRectangle(x, y, width, height, Colours::GrayText, 1.0F);
    }

    if (this->draw_border) {
        ds->DrawRectangle(0.0F, 0.0F, Width, Height, Colours::AccentDark);
    }
}
