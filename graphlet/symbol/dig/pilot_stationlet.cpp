#include "graphlet/symbol/dig/pilot_stationlet.hpp"

#include "datum/flonum.hpp"

#include "brushes.hxx"
#include "polar.hpp"
#include "shape.hpp"
#include "paint.hpp"
#include "math.hpp"

using namespace WarGrey::SCADA;

using namespace Windows::Foundation;

using namespace Microsoft::Graphics::Canvas;
using namespace Microsoft::Graphics::Canvas::Brushes;
using namespace Microsoft::Graphics::Canvas::Geometry;

static CanvasSolidColorBrush^ default_pilot_color = Colours::Crimson;

/*************************************************************************************************/
PilotStationlet::PilotStationlet(float size, ICanvasBrush^ color)
	: width(size), height(size), color((color == nullptr) ? default_pilot_color : color) {
	this->enable_resizing(true);
}

void PilotStationlet::construct() {
	this->construct_pilot_station(false);
}

void PilotStationlet::fill_extent(float x, float y, float* width, float* height) {
	SET_VALUES(width, this->width, height, this->height);
}

void PilotStationlet::draw(CanvasDrawingSession^ ds, float x, float y, float Width, float Height) {
	float cx = x + this->width * 0.5F;
	float cy = y + this->height * 0.5F;
	float rx = cx - x - 1.0F;
	float ry = cy - y - 1.0F;

	ds->DrawCachedGeometry(this->rhombus, cx, cy, this->color);
	ds->DrawEllipse(cx, cy, rx, ry, this->color);
}

void PilotStationlet::construct_pilot_station(bool resized) {
	float radiusX = this->height * 0.5F - 1.0F;
	float radiusY = this->width * 0.5F * 0.618F;

	this->rhombus = geometry_freeze(polar_rhombus(radiusX, radiusY, 0.0));
}

void PilotStationlet::resize(float width, float height) {
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
		this->construct_pilot_station(true);
		this->notify_updated();
	}
}
