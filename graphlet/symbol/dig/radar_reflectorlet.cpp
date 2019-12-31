#include "graphlet/symbol/dig/radar_reflectorlet.hpp"

#include "datum/flonum.hpp"

#include "brushes.hxx"
#include "shape.hpp"
#include "paint.hpp"
#include "math.hpp"

using namespace WarGrey::SCADA;
using namespace WarGrey::DTPM;

using namespace Windows::Foundation;

using namespace Microsoft::Graphics::Canvas;
using namespace Microsoft::Graphics::Canvas::Brushes;
using namespace Microsoft::Graphics::Canvas::Geometry;

static CanvasSolidColorBrush^ default_radar_color = Colours::Crimson;

/*************************************************************************************************/
RadarReflectorlet::RadarReflectorlet(float size, ICanvasBrush^ color)
	: width(size), height(size), color((color == nullptr) ? default_radar_color : color) {
	this->enable_resizing(true);
}

void RadarReflectorlet::construct() {
	this->construct_radar_reflector(false);
}

void RadarReflectorlet::fill_extent(float x, float y, float* width, float* height) {
	SET_VALUES(width, this->width, height, this->height);
}

void RadarReflectorlet::draw(CanvasDrawingSession^ ds, float x, float y, float Width, float Height) {
	ds->DrawCachedGeometry(this->body, x, y, this->color);
}

void RadarReflectorlet::construct_radar_reflector(bool resized) {
	CanvasStrokeStyle^ style = make_roundcap_stroke_style(true);
	double theta = 30.0;
	double start = theta - 180.0;
	double end = -theta;
	float thickness = 1.5F;
	float cx = this->width * 0.5F;
	float cy = this->height * 0.75F;
	float lradius = cx / flcos(degrees_to_radians(theta)) - thickness;
	float sradius = this->width * 0.5F * 0.618F;

	this->body = geometry_freeze(geometry_union(arc(cx, cy, start, end, sradius, sradius, thickness, style),
		radiation(cx, cy, lradius, sradius, start, end, 5, thickness, style)));
}

void RadarReflectorlet::resize(float width, float height) {
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
		this->construct_radar_reflector(true);
		this->notify_updated();
	}
}
