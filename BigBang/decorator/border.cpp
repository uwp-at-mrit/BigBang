#pragma once

#include "decorator/border.hpp"
#include "universe.hpp"
#include "system.hpp"

using namespace WarGrey::SCADA;

using namespace Windows::Foundation;

using namespace Windows::UI;
using namespace Windows::UI::ViewManagement;

using namespace Microsoft::Graphics::Canvas;
using namespace Microsoft::Graphics::Canvas::Brushes;
using namespace Microsoft::Graphics::Canvas::Geometry;

BorderDecorator::BorderDecorator(bool draw_border, bool draw_enclosing) {
    this->draw_border = draw_border;
    this->draw_enclosing_box = draw_enclosing;
}

void BorderDecorator::draw_before(Universe* master, CanvasDrawingSession^ ds, float Width, float Height) {
    if (this->draw_enclosing_box) {
        static auto box_color = ref new CanvasSolidColorBrush(ds, system_color(UIElementType::GrayText));
        float x, y, width, height;

        master->fill_snips_bounds(&x, &y, &width, &height);
        ds->DrawRectangle(x, y, width, height, box_color, 1.0F);
    }

    if (this->draw_border) {
        static auto border_color = ref new CanvasSolidColorBrush(ds, system_color(UIColorType::AccentDark1));
        ds->DrawRectangle(0.0F, 0.0F, Width, Height, border_color);
    }
}
