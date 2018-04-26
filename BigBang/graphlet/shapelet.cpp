#include "graphlet/shapelet.hpp"

#include "paint.hpp"
#include "shape.hpp"
#include "geometry.hpp"

using namespace WarGrey::SCADA;

using namespace Microsoft::Graphics::Canvas;
using namespace Microsoft::Graphics::Canvas::Brushes;
using namespace Microsoft::Graphics::Canvas::Geometry;

/*************************************************************************************************/
IShapelet::IShapelet(CanvasGeometry^ shape, ICanvasBrush^ color, CanvasSolidColorBrush^ border_color, float thickness)
	: color(color), border_color(border_color) {
	this->surface = geometry_freeze(shape);

	if (border_color->Color.A == 0) {
		this->border = nullptr;
		this->box = shape->ComputeBounds();
	} else {
		this->border = geometry_draft(shape, thickness);
		this->box = shape->ComputeStrokeBounds(thickness);
	}
}

void IShapelet::fill_extent(float x, float y, float* w, float* h) {
	SET_VALUES(w, this->box.Width, h, this->box.Height);
}

void IShapelet::draw(CanvasDrawingSession^ ds, float x, float y, float Width, float Height) {
	float px = x - this->box.X;
	float py = y - this->box.Y;
	
	ds->DrawCachedGeometry(this->surface, px, py, this->color);
	if (this->border != nullptr) {
		ds->DrawCachedGeometry(this->border, px, py, this->border_color);
	}
}

/*************************************************************************************************/
Rectanglelet::Rectanglelet(float edge_size, ICanvasBrush^ color, CanvasSolidColorBrush^ border_color, float thickness)
	: IShapelet(rectangle(edge_size, edge_size), color, border_color, thickness) {}

Rectanglelet::Rectanglelet(float width, float height, ICanvasBrush^ color, CanvasSolidColorBrush^ border_color, float thickness)
	: IShapelet(rectangle(width, height), color, border_color, thickness) { }
