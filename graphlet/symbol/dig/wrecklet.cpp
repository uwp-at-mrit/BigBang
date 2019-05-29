#include "graphlet/symbol/dig/wrecklet.hpp"
#include "brushes.hxx"

using namespace WarGrey::SCADA;

using namespace Windows::Foundation;

using namespace Microsoft::Graphics::Canvas;
using namespace Microsoft::Graphics::Canvas::Brushes;


static ICanvasBrush^ default_ship_color = Colours::Snow;
static ICanvasBrush^ default_ring_color = Colours::Red;

/*************************************************************************************************/
SunkenShiplet::SunkenShiplet(float size, ICanvasBrush^ body_color, ICanvasBrush^ ring_color) : width(size), height(size)
	, body_color((body_color == nullptr) ? default_ship_color : body_color)
	, ring_color((ring_color == nullptr) ? default_ring_color : ring_color) {
}

void SunkenShiplet::construct() {
	this->construct_sunken_ship(false);
}

void SunkenShiplet::fill_extent(float x, float y, float* width, float* height) {
	SET_VALUES(width, this->width, height, this->height);
}

void SunkenShiplet::draw(CanvasDrawingSession^ ds, float x, float y, float Width, float Height) {
	float rx = this->width * 0.5F;
	float ry = this->height * 0.5F;
	float half_thick = 1.0F;

	ds->DrawEllipse(x + rx, y + ry, rx - half_thick, ry - half_thick, this->ring_color, half_thick * 2.0F);
}

void SunkenShiplet::construct_sunken_ship(bool resized) {
}

void SunkenShiplet::resize(float width, float height) {
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
		this->construct_sunken_ship(true);
		this->notify_updated();
	}
}

ITurtle* SunkenShiplet::make_sunken_ship_turtle(float width, float height) {
	return nullptr;
}

/*************************************************************************************************/
Wrecklet::Wrecklet(float size, ICanvasBrush^ color) : width(size), height(size), color((color == nullptr) ? default_ship_color : color) {
}

void Wrecklet::construct() {
	this->construct_wreck(false);
}

void Wrecklet::fill_extent(float x, float y, float* width, float* height) {
	SET_VALUES(width, this->width, height, this->height);
}

void Wrecklet::draw(CanvasDrawingSession^ ds, float x, float y, float Width, float Height) {
}

void Wrecklet::construct_wreck(bool resized) {

}

void Wrecklet::resize(float width, float height) {
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
		this->construct_wreck(true);
		this->notify_updated();
	}
}

ITurtle* Wrecklet::make_wreck_turtle(float width, float height) {
	return nullptr;
}
