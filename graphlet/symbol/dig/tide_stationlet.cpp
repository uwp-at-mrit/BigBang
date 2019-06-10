#include "graphlet/symbol/dig/tide_stationlet.hpp"

#include "brushes.hxx"
#include "polar.hpp"

using namespace WarGrey::SCADA;

using namespace Windows::Foundation;

using namespace Microsoft::Graphics::Canvas;
using namespace Microsoft::Graphics::Canvas::Brushes;
using namespace Microsoft::Graphics::Canvas::Geometry;

static CanvasSolidColorBrush^ default_ring_color = Colours::Green;
static CanvasSolidColorBrush^ default_border_color = Colours::SeaGreen;
static CanvasSolidColorBrush^ default_body_color = Colours::ForestGreen;

/*************************************************************************************************/
TideStationlet::TideStationlet(float size, ICanvasBrush^ ring_color, ICanvasBrush^ body_color, ICanvasBrush^ border_color) : width(size), height(size)
, border_color((border_color == nullptr) ? default_border_color : border_color)
, body_color((body_color == nullptr) ? default_body_color : body_color)
, ring_color((ring_color == nullptr) ? default_ring_color : ring_color) {
	this->enable_resizing(true);
}

void TideStationlet::fill_extent(float x, float y, float* width, float* height) {
	SET_VALUES(width, this->width, height, this->height);
}

void TideStationlet::draw(CanvasDrawingSession^ ds, float x, float y, float Width, float Height) {
	float cx = x + this->width * 0.5F;
	float cy = y + this->height * 0.5F;
	float rx = (cx - x) * 0.42F;
	float ry = (cy - y) * 0.42F;

	ds->FillEllipse(cx, cy, rx, ry, this->body_color);
	ds->DrawEllipse(cx, cy, rx, ry, this->ring_color);
	ds->DrawRectangle(x + 0.5F, y + 0.5F, this->width - 1.0F, this->height - 1.0F, this->border_color);
}

void TideStationlet::resize(float width, float height) {
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
		this->notify_updated();
	}
}
