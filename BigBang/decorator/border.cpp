#pragma once

#include "decorator/border.hpp"

#include "planet.hpp"
#include "paint.hpp"
#include "brushes.hxx"

using namespace WarGrey::SCADA;

using namespace Microsoft::Graphics::Canvas;
using namespace Microsoft::Graphics::Canvas::Brushes;

BorderDecorator::BorderDecorator(CanvasSolidColorBrush^ border_color, CanvasSolidColorBrush^ enclosing_color)
	: border_color(border_color), enclosing_color(enclosing_color) {}

void BorderDecorator::draw_before(CanvasDrawingSession^ ds, float Width, float Height) {
	if (this->border_color != nullptr) {
		ds->DrawRectangle(0.0F, 0.0F, Width, Height, this->border_color);
	}
	
	if (this->enclosing_color != nullptr) {
        float x, y, width, height;

        this->fill_graphlets_boundary(&x, &y, &width, &height);
        ds->DrawRectangle(x, y, width, height, this->enclosing_color, 1.0F);
    }
}
