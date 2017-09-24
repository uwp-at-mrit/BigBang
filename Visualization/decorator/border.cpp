#pragma once

#include "decorator/border.hpp"
#include "pasteboard.hxx"
#include "system.hpp"

using namespace WarGrey::SCADA;

using namespace Windows::Foundation;
using namespace Windows::UI::ViewManagement;

using namespace Microsoft::Graphics::Canvas;
using namespace Microsoft::Graphics::Canvas::Brushes;
using namespace Microsoft::Graphics::Canvas::Geometry;

BorderDecorator::BorderDecorator(bool draw_outer, bool draw_inner, bool draw_enclosing) {
    this->draw_outer_border = draw_outer;
    this->draw_inner_border = draw_inner;
    this->draw_enclosing_box = draw_enclosing;
}

void BorderDecorator::draw_before(Pasteboard^ master, CanvasDrawingSession^ ds, float w, float h, Rect& border) {
    if (this->draw_enclosing_box) {
        static auto box_color = ref new CanvasSolidColorBrush(ds, system_color(UIElementType::GrayText));
        float x, y, width, height;

        master->fill_snips_bounds(&x, &y, &width, &height);
        ds->DrawRectangle(x, y, width, height, box_color, 1.0F);
    }
    
    if (this->draw_inner_border) {
        static auto border_color = ref new CanvasSolidColorBrush(ds, system_color(UIColorType::Accent));
        static auto dash_stroke = ref new CanvasStrokeStyle();

        dash_stroke->DashStyle = CanvasDashStyle::Dash;
        ds->DrawRectangle(0.0F, 0.0F, w, h, border_color, 1.0F, dash_stroke);
    }

    if (this->draw_outer_border) {
        static auto border_color = ref new CanvasSolidColorBrush(ds, system_color(UIColorType::AccentDark1));

        ds->DrawRectangle(border, border_color);
    }
}
