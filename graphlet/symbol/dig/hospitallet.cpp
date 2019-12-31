#include "graphlet/symbol/dig/hospitallet.hpp"

#include "brushes.hxx"
#include "shape.hpp"
#include "paint.hpp"

using namespace WarGrey::SCADA;
using namespace WarGrey::DTPM;

using namespace Windows::Foundation;

using namespace Microsoft::Graphics::Canvas;
using namespace Microsoft::Graphics::Canvas::Brushes;
using namespace Microsoft::Graphics::Canvas::Geometry;

static CanvasSolidColorBrush^ default_ring_color = Colours::Snow;
static CanvasSolidColorBrush^ default_cross_color = Colours::Red;

/*************************************************************************************************/
Hospitallet::Hospitallet(float size, ICanvasBrush^ ring_color, ICanvasBrush^ body_color) : width(size), height(size)
, ring_color((ring_color == nullptr) ? default_ring_color : ring_color)
, body_color((body_color == nullptr) ? default_cross_color : body_color) {
	this->enable_resizing(true);
}

void Hospitallet::construct() {
	this->construct_hospital_icon(false);
}

void Hospitallet::fill_extent(float x, float y, float* width, float* height) {
	SET_VALUES(width, this->width, height, this->height);
}

void Hospitallet::draw(CanvasDrawingSession^ ds, float x, float y, float Width, float Height) {
	float cx = x + this->width * 0.5F;
	float cy = y + this->height * 0.5F;
	float rx = cx - x - 1.0F;
	float ry = cy - y - 1.0F;

	ds->DrawCachedGeometry(this->body, cx, cy, this->body_color);
	ds->DrawEllipse(cx, cy, rx, ry, this->ring_color);
}

void Hospitallet::construct_hospital_icon(bool resized) {
	CanvasStrokeStyle^ style = make_roundcap_stroke_style();
	float thickness = 3.0F;
	float hl = this->width - thickness * 2.0F;
	float vl = this->height - thickness * 2.0F;
	float ox = hl * 0.5F;
	float oy = vl * 0.5F;

	this->body = geometry_freeze(geometry_union(vline(0.0F, -oy, vl, thickness, style),
		hline(-ox, 0.0F, hl, thickness, style)));
}

void Hospitallet::resize(float width, float height) {
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
		this->construct_hospital_icon(true);
		this->notify_updated();
	}
}
