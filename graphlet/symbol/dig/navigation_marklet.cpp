#include "graphlet/symbol/dig/navigation_marklet.hpp"

#include "brushes.hxx"
#include "polar.hpp"
#include "shape.hpp"
#include "paint.hpp"

using namespace WarGrey::SCADA;

using namespace Windows::Foundation;

using namespace Microsoft::Graphics::Canvas;
using namespace Microsoft::Graphics::Canvas::Brushes;
using namespace Microsoft::Graphics::Canvas::Geometry;

static CanvasSolidColorBrush^ default_border_color = Colours::Salmon;
static CanvasSolidColorBrush^ default_body_color = Colours::Cyan;

/*************************************************************************************************/
NavigationMarklet::NavigationMarklet(float size, ICanvasBrush^ border_color, ICanvasBrush^ body_color) : width(size), height(size)
, border_color((border_color == nullptr) ? default_border_color : border_color)
, body_color((body_color == nullptr) ? default_body_color : body_color) {
	this->enable_resizing(true);
}

void NavigationMarklet::construct() {
	this->construct_mark_icon(false);
}

void NavigationMarklet::fill_extent(float x, float y, float* width, float* height) {
	SET_VALUES(width, this->width, height, this->height);
}

void NavigationMarklet::draw(CanvasDrawingSession^ ds, float x, float y, float Width, float Height) {
	float cx = x + this->width * 0.5F;
	float cy = y + this->height * 0.5F;

	ds->DrawCachedGeometry(this->body, cx, cy, this->body_color);
	ds->DrawCachedGeometry(this->border, cx, cy, this->border_color);
}

void NavigationMarklet::construct_mark_icon(bool resized) {
	float thickness = 1.0F;
	float cx = this->width * 0.5F;
	float cy = this->height * 0.5F;
	float rx = cx - thickness;
	float ry = cy - thickness;
	CanvasGeometry^ border = polar_triangle(rx, ry, -90.0);
	CanvasGeometry^ sign = ellipse(rx * 0.5F, ry * 0.5F);

	this->border = geometry_freeze(geometry_union(geometry_stroke(border, thickness), geometry_stroke(sign, thickness)));
	this->body = geometry_freeze(border);
}

void NavigationMarklet::resize(float width, float height) {
	bool resized = false;

	if ((width > 0.0F) && (height > 0.0F)) {
		if (this->width != width) {
			this->width = width;
			resized |= true;
		}

		if (this->height != height) {
			this->height = height;
			resized |= true;
		}
	}

	if (resized) {
		this->construct_mark_icon(true);
		this->notify_updated();
	}
}
