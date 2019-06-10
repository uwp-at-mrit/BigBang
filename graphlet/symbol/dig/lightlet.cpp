#include "graphlet/symbol/dig/lightlet.hpp"

#include "datum/flonum.hpp"

#include "brushes.hxx"
#include "shape.hpp"
#include "paint.hpp"
#include "math.hpp"

using namespace WarGrey::SCADA;

using namespace Windows::Foundation;

using namespace Microsoft::Graphics::Canvas;
using namespace Microsoft::Graphics::Canvas::Brushes;
using namespace Microsoft::Graphics::Canvas::Geometry;

static CanvasSolidColorBrush^ default_light_color = Colours::Green;
static CanvasSolidColorBrush^ default_reflection_color = Colours::Azure;

/*************************************************************************************************/
Lightlet::Lightlet(float size, ICanvasBrush^ color, ICanvasBrush^ reflection_color) : width(size), height(size)
, reflection_color((reflection_color == nullptr) ? default_reflection_color : reflection_color)
, color((color == nullptr) ? default_light_color : color) {
	this->enable_resizing(true);
}

void Lightlet::construct() {
	this->construct_light(false);
}

void Lightlet::fill_extent(float x, float y, float* width, float* height) {
	SET_VALUES(width, this->width, height, this->height);
}

void Lightlet::draw(CanvasDrawingSession^ ds, float x, float y, float Width, float Height) {
	float cx = x + this->width * 0.5F;
	float cy = y + this->height * 0.5F;
	float rx = cx - x - 0.5F;
	float ry = cy - y - 0.5F;

	ds->FillEllipse(cx, cy, rx, ry, this->color);
	ds->DrawCachedGeometry(this->reflection, cx, cy, this->reflection_color);
}

void Lightlet::construct_light(bool resized) {
	CanvasStrokeStyle^ style = make_roundcap_stroke_style();
	float rx = this->width * 0.5F * 0.618F;
	float ry = this->width * 0.5F * 0.618F;
	float thickness = 2.0F;

	this->reflection = geometry_freeze(arc(-90.0, -180.0, rx, ry, thickness, style));
}

void Lightlet::resize(float width, float height) {
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
		this->construct_light(true);
		this->notify_updated();
	}
}
