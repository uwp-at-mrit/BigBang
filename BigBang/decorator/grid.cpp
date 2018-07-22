#pragma once

#include <algorithm>

#include "decorator/grid.hpp"
#include "system.hpp"
#include "paint.hpp"
#include "text.hpp"

using namespace WarGrey::SCADA;

using namespace Windows::Foundation;

using namespace Windows::UI;
using namespace Windows::UI::ViewManagement;

using namespace Microsoft::Graphics::Canvas;
using namespace Microsoft::Graphics::Canvas::Brushes;

GridDecorator::GridDecorator(float w, float h, float x, float y) : width(w), height(h), x0(x), y0(y) {
	if (this->width <= 0.0F) {
		this->width = 16.0F;
	}

    if (this->height <= 0.0F) {
        this->height = this->width;
    }
}

void GridDecorator::draw_before(CanvasDrawingSession^ ds, float Width, float Height) {
    static auto grid_color = make_solid_brush(system_color(UIElementType::GrayText));
	static auto font = make_text_format(fmin(this->width, this->height) * 0.42F);
	size_t idx = 0;

    grid_color->Opacity = 0.64F;
    for (float x = this->x0; x <= Width; x += this->width) {
        ds->DrawLine(x, 0.0F, x, Height, grid_color);
		ds->DrawText(idx.ToString(), x, this->y0, grid_color, font);
		idx++;
    }

	idx = 0;
    for (float y = this->y0; y <= Height; y += this->height) {
        ds->DrawLine(0.0F, y, Width, y, grid_color);
		ds->DrawText(idx.ToString(), this->x0, y, grid_color, font);
		idx++;
	}
}
