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

GridDecorator::GridDecorator(float w, float h, float x, float y) : gwidth(w), gheight(h), x0(x), y0(y) {
	if (this->gwidth <= 0.0F) {
		this->gwidth = 16.0F;
	}

    if (this->gheight <= 0.0F) {
        this->gheight = this->gwidth;
    }
}

float GridDecorator::get_grid_width() {
	return this->gwidth;
}

void GridDecorator::set_grid_width(float new_width, float start_x) {
	this->x0 = start_x;

	if (new_width >= 0.0F) {
		this->gwidth = new_width;
	}
}

float GridDecorator::get_grid_height() {
	return this->gheight;
}

void GridDecorator::set_grid_height(float new_height, float start_y) {
	this->y0 = start_y;

	if (new_height >= 0.0F) {
		this->gheight = new_height;
	}
}

void GridDecorator::draw_before(CanvasDrawingSession^ ds, float Width, float Height) {
    static auto grid_color = make_solid_brush(system_color(UIElementType::GrayText));
	static auto font = make_text_format(fminf(this->gwidth, this->gheight) * 0.42F);
	size_t idx = 0;

    grid_color->Opacity = 0.64F;
    for (float x = this->x0; x <= Width; x += this->gwidth) {
        ds->DrawLine(x, 0.0F, x, Height, grid_color);
		ds->DrawText(idx.ToString(), x, this->y0, grid_color, font);
		idx++;
    }

	idx = 0;
    for (float y = this->y0; y <= Height; y += this->gheight) {
        ds->DrawLine(0.0F, y, Width, y, grid_color);
		ds->DrawText(idx.ToString(), this->x0, y, grid_color, font);
		idx++;
	}
}
