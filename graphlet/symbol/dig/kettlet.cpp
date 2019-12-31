#include "graphlet/symbol/dig/kettlet.hpp"

#include "brushes.hxx"
#include "polar.hpp"

using namespace WarGrey::SCADA;
using namespace WarGrey::DTPM;

using namespace Windows::Foundation;

using namespace Microsoft::Graphics::Canvas;
using namespace Microsoft::Graphics::Canvas::Brushes;
using namespace Microsoft::Graphics::Canvas::Geometry;

static CanvasSolidColorBrush^ default_ring_color = Colours::SeaGreen;
static CanvasSolidColorBrush^ default_border_color = Colours::Green;
static CanvasSolidColorBrush^ default_body_color = Colours::ForestGreen;

/*************************************************************************************************/
Kettlet::Kettlet(float size, ICanvasBrush^ ring_color, ICanvasBrush^ body_color, ICanvasBrush^ border_color) : width(size), height(size)
, border_color((border_color == nullptr) ? default_border_color : border_color)
, body_color((body_color == nullptr) ? default_body_color : body_color)
, ring_color((ring_color == nullptr) ? default_ring_color : ring_color) {
	this->enable_resizing(true);
}

void Kettlet::construct() {
	this->construct_kettle(false);
}

void Kettlet::fill_extent(float x, float y, float* width, float* height) {
	SET_VALUES(width, this->width, height, this->height);
}

void Kettlet::draw(CanvasDrawingSession^ ds, float x, float y, float Width, float Height) {
	float cx = x + this->width * 0.5F;
	float cy = y + this->height * 0.5F;
	float rx = cx - x - 1.0F;
	float ry = (cy - y) * 0.75F;

	ds->FillGeometry(this->kettle, cx, cy, this->body_color);
	ds->DrawGeometry(this->kettle, cx, cy, this->border_color);
	ds->DrawEllipse(cx, cy, rx, ry, this->ring_color);
}

void Kettlet::construct_kettle(bool resized) {
	float rx = this->width * 0.5F - 2.0F;
	float ry = this->height * 0.5F * 0.75F - 2.0F;

	this->kettle = polar_rectangle(rx, ry, 45.0, 0.0);
}

void Kettlet::resize(float width, float height) {
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
		this->construct_kettle(true);
		this->notify_updated();
	}
}
