#include "graphlet/symbol/dig/rocklet.hpp"

#include "brushes.hxx"
#include "polar.hpp"

using namespace WarGrey::SCADA;
using namespace WarGrey::DTPM;

using namespace Windows::Foundation;

using namespace Microsoft::Graphics::Canvas;
using namespace Microsoft::Graphics::Canvas::Brushes;
using namespace Microsoft::Graphics::Canvas::Geometry;

static CanvasSolidColorBrush^ default_rock_color = Colours::WhiteSmoke;

/*************************************************************************************************/
Rocklet::Rocklet(float size, ICanvasBrush^ color) : width(size), height(size), color((color == nullptr) ? default_rock_color : color) {
	this->enable_resizing(true);
}

void Rocklet::fill_extent(float x, float y, float* width, float* height) {
	SET_VALUES(width, this->width, height, this->height);
}

void Rocklet::draw(CanvasDrawingSession^ ds, float x, float y, float Width, float Height) {
	float cx = x + this->width * 0.5F;
	float cy = y + this->height * 0.5F;
	float rx = x + this->width;
	float by = y + this->height;

	ds->DrawLine(x, cy, rx, cy, this->color);
	ds->DrawLine(cx, y, cx, by, this->color);
	ds->DrawLine(x, y, rx, by, this->color);
	ds->DrawLine(rx, y, x, by, this->color);
}

void Rocklet::resize(float width, float height) {
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
