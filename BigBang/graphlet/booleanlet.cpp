#include "graphlet/booleanlet.hpp"

#include "box.hpp"

using namespace WarGrey::SCADA;

using namespace Microsoft::Graphics::Canvas;
using namespace Microsoft::Graphics::Canvas::Brushes;

Booleanlet::Booleanlet(float size, ICanvasBrush^ true_color, ICanvasBrush^ false_color)
	: size(size), true_color(true_color), false_color(false_color) {}

void Booleanlet::fill_extent(float x, float y, float* w, float* h) {
	SET_BOXES(w, h, this->size);
}

void Booleanlet::draw(CanvasDrawingSession^ ds, float x, float y, float Width, float Height) {
	auto color = (this->get_scale() ? this->true_color : this->false_color);

	ds->FillRectangle(x, y, this->size, this->size, color);
}
