#pragma once

#include "decorator/border.hpp"
#include "pasteboard.hxx"
#include "system.hpp"

using namespace WarGrey::SCADA;

using namespace Windows::Foundation;

using namespace Windows::UI;
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

/*************************************************************************************************/
HBorderDecorator::HBorderDecorator(bool draw_top, bool draw_bottom, Color& color) : border_color(color) {
    this->draw_top_border = draw_top;
    this->draw_bottom_border = draw_bottom;
}

void HBorderDecorator::draw_before(Pasteboard^ master, CanvasDrawingSession^ ds, float w, float h, Rect& border) {
    if (this->draw_top_border) {
        ds->DrawLine(border.X, border.Y, border.X + border.Width, border.Y, this->border_color, 2.0F);
    }
    
    if (this->draw_bottom_border) {
        auto y = border.Y + border.Height;
        ds->DrawLine(border.X, y, border.X + border.Width, y, this->border_color, 2.0F);
    }
}

/**************************************************************************************************/
VBorderDecorator::VBorderDecorator(bool draw_left, bool draw_right, Color& color) : border_color(color) {
    this->draw_left_border = draw_left;
    this->draw_right_border = draw_right;
}

void VBorderDecorator::draw_before(Pasteboard^ master, CanvasDrawingSession^ ds, float w, float h, Rect& border) {
    if (this->draw_left_border) {
        ds->DrawLine(border.X, border.Y, border.X, border.Y + border.Height, this->border_color, 2.0F);
    }

    if (this->draw_right_border) {
        auto x = border.X + border.Width;
        ds->DrawLine(x, border.Y, x, border.Y + border.Height, this->border_color, 2.0F);
    }
}
