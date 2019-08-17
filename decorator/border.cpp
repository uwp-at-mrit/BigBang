#pragma once

#include "decorator/border.hpp"

using namespace WarGrey::SCADA;

using namespace Microsoft::Graphics::Canvas;
using namespace Microsoft::Graphics::Canvas::Brushes;

BorderDecorator::BorderDecorator(CanvasSolidColorBrush^ border_color, CanvasSolidColorBrush^ enclosing_color)
	: border_color(border_color), enclosing_color(enclosing_color) {}

void BorderDecorator::draw_before(CanvasDrawingSession^ ds, float X, float Y, float Width, float Height) {
	if (this->border_color != nullptr) {
		ds->DrawRectangle(X, Y, Width, Height, this->border_color);
	}
	
	if (this->enclosing_color != nullptr) {
        float x, y, width, height;

        this->fill_graphlets_boundary(&x, &y, &width, &height);
        ds->DrawRectangle(x + X, y + Y, width, height, this->enclosing_color, 1.0F);
    }
}
