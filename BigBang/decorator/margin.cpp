#pragma once

#include "decorator/margin.hpp"

#include "graphlet/primitive.hpp"
#include "brushes.hxx"

using namespace WarGrey::SCADA;

using namespace Microsoft::Graphics::Canvas;

MarginDecorator::MarginDecorator(bool selected_only, bool draw_border) {
    this->draw_border = draw_border;
	this->draw_all = !selected_only;
}

void MarginDecorator::draw_after_graphlet(IGraphlet* g, CanvasDrawingSession^ ds, float x, float y, float width, float height, bool selected) {
	if (this->draw_all || selected) {
		float ts, rs, bs, ls;

		g->fill_margin(x, y, &ts, &rs, &bs, &ls);
		ds->DrawRectangle(x + ls, y + ts, width - ls - rs, height - ts - bs, Colours::GrayText, 1.0F);

		if (this->draw_border) {
			ds->DrawRectangle(x, y, width, height, Colours::AccentDark);
		}
	}
}
