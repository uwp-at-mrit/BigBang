#include "graphlet/symbol/dig/wrecklet.hpp"
#include "brushes.hxx"

using namespace WarGrey::SCADA;

using namespace Windows::Foundation;

using namespace Microsoft::Graphics::Canvas;
using namespace Microsoft::Graphics::Canvas::Brushes;

static const float wreck_base_size = 16.0F;

static ICanvasBrush^ default_ship_color = Colours::Snow;
static ICanvasBrush^ default_ring_color = Colours::Red;

private enum class W {
	Home, Deck, Level, lLevel, rLevel,
	_,
	ll, rl, ld, rd, l
};

/*************************************************************************************************/
SunkenShiplet::SunkenShiplet(float size, ICanvasBrush^ body_color, ICanvasBrush^ ring_color) : width(size), height(size)
	, body_color((body_color == nullptr) ? default_ship_color : body_color)
	, ring_color((ring_color == nullptr) ? default_ring_color : ring_color) {
	this->enable_resizing(true);
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
	ds->DrawCachedGeometry(this->body, x, y, this->body_color);
}

void SunkenShiplet::construct_sunken_ship(bool resized) {
	ITurtle* turtle = this->make_sunken_ship_turtle(this->width, this->height);

	this->body = geometry_freeze(turtle->snap_track());

	turtle->destroy();
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
	Turtle<W>* turtle = new Turtle<W>(width / wreck_base_size, height / wreck_base_size, true);

	turtle->reference();

	turtle->jump_right(wreck_base_size * 0.5F)->jump_down(4.0F, W::Home)->jump_down(0.5F);
	turtle->move_down(5.0F, W::Deck)->move_down(2.0F, W::Level)->move_right(2.0F);
	turtle->jump_right_down(0.5F)->move_down(1.0F, W::rLevel)->jump_left_down(0.5F)->move_left(4.0F);
	turtle->jump_left_up(0.5F, W::lLevel)->move_up(1.0F)->jump_right_up(0.5F)->move_to(W::Level);

	turtle->jump_back(W::Home)->jump_left(0.5F)->move_right_up(0.5F, W::l);
	turtle->move_right_up(1.5F)->jump_back()->move_left_up(2.0F)->jump_back();
	turtle->move_right_down(0.5F)->move_left_down(0.5F)->move_left_up(0.5F);
	turtle->move_left_down(1.0F);

	turtle->jump_back(W::lLevel)->move_left(wreck_base_size * 0.5F);
	turtle->jump_back(W::rLevel)->move_right(wreck_base_size * 0.5F);

	turtle->jump_back(W::Deck)->move_left(3.0F)->move_up(0.5F)->jump_left_up(0.5F)->move_left(1.5F, W::ll)->move_up(4.0F);
	turtle->jump_back()->move_left(2.0F)->jump_right(0.5F)->move_down(1.5F);
	turtle->jump_right_down(0.5F, W::ld)->move_right(2.5F)->jump_back()->move_down(2.0F);

	turtle->jump_back(W::Deck)->move_right(3.0F)->move_up(0.5F)->jump_right_up(0.5F)->move_right(1.5F, W::rl)->move_up(4.0F);
	turtle->jump_back()->move_right(2.0F)->jump_left(0.5F)->move_down(1.5F);
	turtle->jump_left_down(0.5F, W::ld)->move_left(2.5F)->jump_back()->move_down(2.0F);

	return turtle;
}

/*************************************************************************************************/
Wrecklet::Wrecklet(float size, ICanvasBrush^ color) : width(size), height(size), color((color == nullptr) ? default_ship_color : color) {
	this->enable_resizing(true);
}

void Wrecklet::construct() {
	this->construct_wreck(false);
}

void Wrecklet::fill_extent(float x, float y, float* width, float* height) {
	SET_VALUES(width, this->width, height, this->height);
}

void Wrecklet::draw(CanvasDrawingSession^ ds, float x, float y, float Width, float Height) {
	ds->DrawCachedGeometry(this->body, x, y, this->color);
}

void Wrecklet::construct_wreck(bool resized) {
	ITurtle* turtle = this->make_wreck_turtle(this->width, this->height);

	this->body = geometry_freeze(turtle->snap_path());

	turtle->destroy();
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
	Turtle<W>* turtle = new Turtle<W>(width / wreck_base_size, height / wreck_base_size, true);

	turtle->reference();

	turtle->jump_down(wreck_base_size * 0.5F)->jump_right(2.0F, W::Home);
	turtle->jump_right(3.0F)->jump_down(6.0F, W::ll)->move_to(W::Home)->jump_back();
	turtle->move_left(5.0F)->move_down(2.0F)->move_right(wreck_base_size)->move_up(2.0F);
	turtle->move_left(4.0F)->move_up(1.0F)->move_left(2.0F)->move_left_up(3.0F, W::l);
	turtle->move_left_down(1.0F)->move_to(W::Home)->jump_back();
	
	turtle->move_up(2.0F)->move_right(1.0F)->move_up(4.0F)->move_right(2.0F);
	turtle->move_down(4.0F)->move_left(1.0F)->move_down(4.0F)->move_to(W::l);

	return turtle;
}
