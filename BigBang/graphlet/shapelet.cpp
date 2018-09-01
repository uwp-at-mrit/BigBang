#include "graphlet/shapelet.hpp"

#include "paint.hpp"
#include "shape.hpp"
#include "geometry.hpp"

using namespace WarGrey::SCADA;

using namespace Microsoft::Graphics::Canvas;
using namespace Microsoft::Graphics::Canvas::Brushes;
using namespace Microsoft::Graphics::Canvas::Geometry;

/*************************************************************************************************/
IShapelet::IShapelet(CanvasGeometry^ shape, ICanvasBrush^ color, CanvasSolidColorBrush^ bcolor
	, float thickness, CanvasStrokeStyle^ style) : color(color), border_color(bcolor) {
	this->surface = geometry_freeze(shape);
	this->border = geometry_draft(shape, thickness, style);

	this->box = shape->ComputeBounds();
	this->border_box = shape->ComputeStrokeBounds(thickness);
}

void IShapelet::construct() {
	this->set_color(this->color);
	this->set_border_color(this->border_color);
}

void IShapelet::fill_extent(float x, float y, float* w, float* h) {
	if (this->border_color == nullptr) {
		SET_VALUES(w, this->box.Width, h, this->box.Height);
	} else {
		SET_VALUES(w, this->border_box.Width, h, this->border_box.Height);
	}
}

void IShapelet::fill_shape_origin(float* x, float* y) {
	if (this->border_color == nullptr) {
		SET_VALUES(x, this->box.X, y, this->box.Y);
	} else {
		SET_VALUES(x, this->border_box.X, y, this->border_box.Y);
	}
}

void IShapelet::set_border_color(CanvasSolidColorBrush^ color) {
	this->border_color = color;

	if ((color != nullptr) && (color->Color.A == 0)) {
		this->border_color = nullptr;
	}
}

void IShapelet::set_color(ICanvasBrush^ color) {
	this->color = ((color == nullptr) ? Colours::Background : color);
}

void IShapelet::draw(CanvasDrawingSession^ ds, float x, float y, float Width, float Height) {
	float ox, oy;
	
	this->fill_shape_origin(&ox, &oy);
	
	ds->DrawCachedGeometry(this->surface, x - ox, y - oy, this->color);

	if (this->border_color != nullptr) {
		ds->DrawCachedGeometry(this->border, x - ox, y - oy, this->border_color);
	}
}

/*************************************************************************************************/
Rectanglet::Rectanglet(float edge_size, ICanvasBrush^ color, CanvasSolidColorBrush^ border_color, float thickness)
	: IShapelet(rectangle(edge_size, edge_size), color, border_color, thickness) {}

Rectanglet::Rectanglet(float width, float height, ICanvasBrush^ color, CanvasSolidColorBrush^ border_color, float thickness)
	: IShapelet(rectangle(width, height), color, border_color, thickness) {}

/*************************************************************************************************/
Trianglet::Trianglet(float edge_size, ICanvasBrush^ color, CanvasSolidColorBrush^ border_color, float thickness)
	: IShapelet(rectangle(edge_size, edge_size), color, border_color, thickness) {}

Trianglet::Trianglet(float width, float height, ICanvasBrush^ color, CanvasSolidColorBrush^ border_color, float thickness)
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
