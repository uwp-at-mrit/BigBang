#include "graphlet/shapelet.hpp"

#include "paint.hpp"
#include "shape.hpp"
#include "geometry.hpp"

using namespace WarGrey::SCADA;

using namespace Microsoft::Graphics::Canvas;
using namespace Microsoft::Graphics::Canvas::Brushes;
using namespace Microsoft::Graphics::Canvas::Geometry;

/*************************************************************************************************/
IShapelet::IShapelet(CanvasGeometry^ shape, ICanvasBrush^ color, CanvasSolidColorBrush^ border_color
	, float thickness, CanvasStrokeStyle^ style) : color(color), border_color(border_color) {
	this->surface = geometry_freeze(shape);

	if (border_color->Color.A == 0) {
		this->border = nullptr;
		this->box = shape->ComputeBounds();
	} else {
		this->border = geometry_draft(shape, thickness, style);
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
	: IShapelet(rectangle(width, height), color, border_color, thickness) {}

/*************************************************************************************************/
HLinelet::HLinelet(float thickness, CanvasSolidColorBrush^ color, CanvasStrokeStyle^ style)
	: thickness(thickness), color(color), style(style) {}

void HLinelet::fill_extent(float x, float y, float* width, float* height) {
	SET_BOX(width, this->available_visible_width(x));
	SET_BOX(height, this->thickness);
}

void HLinelet::draw(CanvasDrawingSession^ ds, float x, float y, float Width, float Height) {
	float line_y = y + this->thickness * 0.5F;

	ds->DrawLine(x, line_y, x + Width, line_y, this->color, this->thickness, this->style);
}

VLinelet::VLinelet(float thickness, CanvasSolidColorBrush^ color, CanvasStrokeStyle^ style)
	: thickness(thickness), color(color), style(style) {}

void VLinelet::fill_extent(float x, float y, float* width, float* height) {
	SET_BOX(width, this->thickness);
	SET_BOX(height, this->available_visible_height(y));
}

void VLinelet::draw(CanvasDrawingSession^ ds, float x, float y, float Width, float Height) {
	float line_x = x + this->thickness * 0.5F;

	ds->DrawLine(line_x, y, line_x, y + Height, this->color, this->thickness, this->style);
}
