#include "graphlet/symbol/dig/picketlet.hpp"

#include "brushes.hxx"
#include "shape.hpp"

using namespace WarGrey::SCADA;
using namespace WarGrey::DTPM;

using namespace Windows::Foundation;

using namespace Microsoft::Graphics::Canvas;
using namespace Microsoft::Graphics::Canvas::Brushes;
using namespace Microsoft::Graphics::Canvas::Geometry;

static CanvasSolidColorBrush^ default_border_color = Colours::SeaGreen;
static CanvasSolidColorBrush^ default_picket_color = Colours::Burlywood;

/*************************************************************************************************/
Picketlet::Picketlet(float size, ICanvasBrush^ border_color, ICanvasBrush^ picket_color) : width(size), height(size)
, border_color((border_color == nullptr) ? default_border_color : border_color)
, picket_color((picket_color == nullptr) ? default_picket_color : picket_color) {
	this->enable_resizing(true);
}

void Picketlet::fill_extent(float x, float y, float* width, float* height) {
	SET_VALUES(width, this->width, height, this->height);
}

void Picketlet::draw(CanvasDrawingSession^ ds, float x, float y, float Width, float Height) {
	float cx = x + this->width * 0.5F;
	float cy = y + this->height * 0.5F;
	float brx = cx - x - 1.0F;
	float bry = cy - y - 1.0F;
	float prx = brx * 0.500F;
	float pry = bry * 0.618F;

	ds->DrawEllipse(cx, cy, brx, bry, this->border_color);
	ds->DrawEllipse(cx, cy, prx, pry, this->picket_color);
}

void Picketlet::resize(float width, float height) {
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
