#include "graphlet/shapelet.hpp"

#include "paint.hpp"
#include "geometry.hpp"

using namespace WarGrey::SCADA;

using namespace Windows::UI;

using namespace Microsoft::Graphics::Canvas;
using namespace Microsoft::Graphics::Canvas::Brushes;
using namespace Microsoft::Graphics::Canvas::Geometry;

Geometrylet::Geometrylet(CanvasGeometry^ shape, ICanvasBrush^ color, CanvasSolidColorBrush^ border_color, float thickness)
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

void Geometrylet::fill_extent(float x, float y, float* w, float* h) {
	SET_VALUES(w, this->box.Width, h, this->box.Height);
}

void Geometrylet::draw(CanvasDrawingSession^ ds, float x, float y, float Width, float Height) {
	float px = x - this->box.X;
	float py = y - this->box.Y;
	
	ds->DrawCachedGeometry(this->surface, px, py, this->color);
	if (this->border != nullptr) {
		ds->DrawCachedGeometry(this->border, px, py, this->border_color);
	}
}
