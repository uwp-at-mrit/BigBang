#include "shapelet.hpp"

#include "paint.hpp"
#include "geometry.hpp"

using namespace WarGrey::SCADA;

using namespace Windows::UI;

using namespace Microsoft::Graphics::Canvas;
using namespace Microsoft::Graphics::Canvas::Brushes;
using namespace Microsoft::Graphics::Canvas::Geometry;

Shapelet::Shapelet(CanvasGeometry^ shape, Color& color, Color& border_color, float thickness) {
	this->surface = geometry_freeze(shape);
	this->color = make_solid_brush(color);

	if (border_color.A == 0) {
		this->border = nullptr;
		this->border_color = transparent_brush();
		this->box = shape->ComputeStrokeBounds(1.0);
	} else {
		this->border = geometry_draft(shape, thickness);
		this->border_color = make_solid_brush(border_color);
		this->box = shape->ComputeStrokeBounds(thickness);
	}
}

void Shapelet::fill_extent(float x, float y, float* w, float* h) {
	SET_VALUES(w, this->box.Width, h, this->box.Height);
}

void Shapelet::fill_extent_offset(float* x, float* y) {
	SET_VALUES(x, this->box.X, y, this->box.Y);
}

void Shapelet::draw(CanvasDrawingSession^ ds, float x, float y, float Width, float Height) {
	float px = x - this->box.X;
	float py = y - this->box.Y;
	
	ds->DrawCachedGeometry(this->surface, px, py, this->color);
	if (this->border != nullptr) {
		ds->DrawCachedGeometry(this->border, px, py, this->border_color);
	}
}

/*************************************************************************************************/
Pipelinelet::Pipelinelet(Platform::Array<TurtleMove>^ moves, float usize, float thickness, Color& color)
	: Shapelet(traceline(moves, usize, thickness), color) {
}
