#include "graphlet/symbol/dig/report_spotlet.hpp"

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

static CanvasSolidColorBrush^ default_spot_color = Colours::Crimson;

/*************************************************************************************************/
ReportSpotlet::ReportSpotlet(float size, ICanvasBrush^ color)
	: width(size), height(size), color((color == nullptr) ? default_spot_color : color) {
	this->enable_resizing(true);
}

void ReportSpotlet::construct() {
	this->construct_pilot_station(false);
}

void ReportSpotlet::fill_extent(float x, float y, float* width, float* height) {
	SET_VALUES(width, this->width, height, this->height);
}

void ReportSpotlet::draw(CanvasDrawingSession^ ds, float x, float y, float Width, float Height) {
	float cx = x + this->width * 0.5F;
	float cy = y + this->height * 0.5F;

	ds->DrawCachedGeometry(this->rhombus, cx, cy, this->color);
	ds->DrawCachedGeometry(this->spot, cx, cy, this->color);
}

void ReportSpotlet::construct_pilot_station(bool resized) {
	float radiusX = this->height * 0.5F * 0.618F;
	float radiusY = this->width * 0.5F - 1.0F;
	float d = radiusX * 2.0F - 2.0F;
	float D = radiusY * 2.0F - 2.0F;
	float inscribed_r = (D * d) / (2.0F * flsqrt(d * d + D * D));

	this->rhombus = geometry_draft(polar_rhombus(radiusX, radiusY, 0.0));
	this->spot = geometry_draft(circle(inscribed_r));
}

void ReportSpotlet::resize(float width, float height) {
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
