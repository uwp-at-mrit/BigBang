#include "graphlet/shapelet.hpp"

#include "paint.hpp"
#include "shape.hpp"
#include "polar.hpp"
#include "geometry.hpp"

using namespace WarGrey::SCADA;

using namespace Microsoft::Graphics::Canvas;
using namespace Microsoft::Graphics::Canvas::Brushes;
using namespace Microsoft::Graphics::Canvas::Geometry;

/*************************************************************************************************/
Shapelet::Shapelet(CanvasGeometry^ shape, ICanvasBrush^ color, CanvasSolidColorBrush^ bcolor
	, float thickness, CanvasStrokeStyle^ style) : color(color), border_color(bcolor) {
	this->surface = geometry_freeze(shape);
	this->border = geometry_draft(shape, thickness, style);

	this->box = shape->ComputeBounds();
	this->border_box = shape->ComputeStrokeBounds(thickness);
}

void Shapelet::construct() {
	this->set_color(this->color);
	this->set_border_color(this->border_color);
}

void Shapelet::fill_extent(float x, float y, float* w, float* h) {
	if (this->border_color == nullptr) {
		SET_VALUES(w, this->box.Width, h, this->box.Height);
	} else {
		SET_VALUES(w, this->border_box.Width, h, this->border_box.Height);
	}
}

void Shapelet::fill_shape_origin(float* x, float* y) {
	if (this->border_color == nullptr) {
		SET_VALUES(x, this->box.X, y, this->box.Y);
	} else {
		SET_VALUES(x, this->border_box.X, y, this->border_box.Y);
	}
}

void Shapelet::set_border_color(unsigned int color) {
	this->set_border_color(Colours::make(color));
}

void Shapelet::set_border_color(CanvasSolidColorBrush^ color) {
	this->border_color = color;

	if ((color != nullptr) && (color->Color.A == 0)) {
		this->border_color = nullptr;
	}
}

void Shapelet::set_color(unsigned int color) {
	this->set_color(Colours::make(color));
}

void Shapelet::set_color(ICanvasBrush^ color) {
	this->color = ((color == nullptr) ? Colours::Background : color);
}

void Shapelet::draw(CanvasDrawingSession^ ds, float x, float y, float Width, float Height) {
	float ox, oy;
	
	this->fill_shape_origin(&ox, &oy);
	
	ds->DrawCachedGeometry(this->surface, x - ox, y - oy, this->color);

	if (this->border_color != nullptr) {
		ds->DrawCachedGeometry(this->border, x - ox, y - oy, this->border_color);
	}
}

/*************************************************************************************************/
Rectanglet::Rectanglet(float edge_size, unsigned int border_color, float thickness)
	: Rectanglet(edge_size, nullptr, Colours::make(border_color), thickness) {}

Rectanglet::Rectanglet(float width, float height, unsigned int border_color, float thickness)
	: Rectanglet(width, height, nullptr, Colours::make(border_color), thickness) {}

Rectanglet::Rectanglet(float edge_size, ICanvasBrush^ color, CanvasSolidColorBrush^ border_color, float thickness)
	: Shapelet(rectangle(edge_size, edge_size), color, border_color, thickness) {}

Rectanglet::Rectanglet(float width, float height, ICanvasBrush^ color, CanvasSolidColorBrush^ border_color, float thickness)
	: Shapelet(rectangle(width, height), color, border_color, thickness) {}

/*************************************************************************************************/
Circlelet::Circlelet(float radius, unsigned int border_color, float thickness)
	: Circlelet(radius, nullptr, Colours::make(border_color), thickness) {}

Circlelet::Circlelet(float radius, ICanvasBrush^ color, CanvasSolidColorBrush^ border_color, float thickness)
	: Shapelet(circle(radius), color, border_color, thickness) {}

Sectorlet::Sectorlet(double sdegrees, double edegrees, float radiusX, float radiusY, unsigned int border_color, float thickness)
	: Sectorlet(sdegrees, edegrees, radiusX, radiusY, nullptr, Colours::make(border_color), thickness) {}

Sectorlet::Sectorlet(double sdegrees, double edegrees, float radiusX, float radiusY
	, ICanvasBrush^ color, CanvasSolidColorBrush^ border_color, float thickness)
	: Shapelet(sector(sdegrees, edegrees, radiusX, radiusY), color, border_color, thickness) {}

Segmentlet::Segmentlet(double sdegrees, double edegrees, float radiusX, float radiusY, unsigned int border_color, float thickness)
	: Segmentlet(sdegrees, edegrees, radiusX, radiusY, nullptr, Colours::make(border_color), thickness) {}

Segmentlet::Segmentlet(double sdegrees, double edegrees, float radiusX, float radiusY
	, ICanvasBrush^ color, CanvasSolidColorBrush^ border_color, float thickness)
	: Shapelet(segment(sdegrees, edegrees, radiusX, radiusY), color, border_color, thickness) {}

/*************************************************************************************************/
ArrowHeadlet::ArrowHeadlet(float radius, double degrees, unsigned int border_color, float thickness)
	: ArrowHeadlet(radius, degrees, nullptr, Colours::make(border_color), thickness) {}

ArrowHeadlet::ArrowHeadlet(float radius, double degrees, ICanvasBrush^ color, CanvasSolidColorBrush^ border_color, float thickness)
	: Shapelet(polar_arrowhead(radius, degrees), color, border_color, thickness), start_degrees(degrees) {}

double ArrowHeadlet::get_degrees() {
	return this->start_degrees;
}

/*************************************************************************************************/
Linelet::Linelet(float sx, float sy, float ex, float ey, float thickness, unsigned int color, CanvasStrokeStyle^ style)
	: Linelet(sx, sy, ex, ey, thickness, Colours::make(color), style) {}

Linelet::Linelet(float sx, float sy, float ex, float ey, float thickness, CanvasSolidColorBrush^ color, CanvasStrokeStyle^ style)
	: Shapelet(line(sx, sy, ex, ey, thickness, style), color) {}

Arclet::Arclet(double sdegrees, double edegrees, float rx, float ry, float thickness, unsigned int color, CanvasStrokeStyle^ style)
	: Arclet(sdegrees, edegrees, rx, ry, thickness, Colours::make(color), style) {}

Arclet::Arclet(double sdegrees, double edegrees, float rx, float ry, float thickness, CanvasSolidColorBrush^ color, CanvasStrokeStyle^ style)
	: Shapelet(arc(sdegrees, edegrees, rx, ry, thickness, style), color) {}

Omegalet::Omegalet(double sdegrees, float r, float thickness, unsigned int color, CanvasStrokeStyle^ style)
	: Omegalet(sdegrees, r, thickness, Colours::make(color), style) {}

Omegalet::Omegalet(double sdegrees, float r, float thickness, CanvasSolidColorBrush^ color, CanvasStrokeStyle^ style)
	: Shapelet(omega(sdegrees, r, thickness, style, -(r * 2.0F + thickness) / r), color) {}

/*************************************************************************************************/
HLinelet::HLinelet(float thickness, unsigned int color, CanvasStrokeStyle^ style)
	: HLinelet(thickness, Colours::make(color), style) {}

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

VLinelet::VLinelet(float thickness, unsigned int color, CanvasStrokeStyle^ style)
	: VLinelet(thickness, Colours::make(color), style) {}

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
