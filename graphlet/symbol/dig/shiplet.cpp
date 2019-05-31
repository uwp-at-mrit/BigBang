#include "graphlet/symbol/dig/shiplet.hpp"
#include "brushes.hxx"

using namespace WarGrey::SCADA;

using namespace Windows::Foundation;

using namespace Microsoft::Graphics::Canvas;
using namespace Microsoft::Graphics::Canvas::Brushes;

static const float ship_icon_base_size = 16.0F;

static ICanvasBrush^ default_ship_color = Colours::Snow;
static ICanvasBrush^ default_ring_color = Colours::Red;
static ICanvasBrush^ default_light_color = Colours::Tomato;

private enum class S {
	Home, Deck, Level, lLevel, rLevel,
	_,
	ll, rl, ld, rd, l
};

/*************************************************************************************************/
LightShiplet::LightShiplet(float size, ICanvasBrush^ body_color, ICanvasBrush^ ring_color) : width(size), height(size)
, body_color((body_color == nullptr) ? default_ship_color : body_color)
, ring_color((ring_color == nullptr) ? default_light_color : ring_color) {
	this->enable_resizing(true);
}

void LightShiplet::construct() {
	this->construct_light_ship(false);
}

void LightShiplet::fill_extent(float x, float y, float* width, float* height) {
	SET_VALUES(width, this->width, height, this->height);
}

void LightShiplet::draw(CanvasDrawingSession^ ds, float x, float y, float Width, float Height) {
	float rx = this->width * 0.5F;
	float ry = this->height * 0.5F;
	float half_thick = 1.0F;

	ds->DrawEllipse(x + rx, y + ry, rx - half_thick, ry - half_thick, this->ring_color, half_thick * 2.0F);
	ds->DrawCachedGeometry(this->body, x, y, this->body_color);
}

void LightShiplet::construct_light_ship(bool resized) {
	ITurtle* turtle = this->make_light_ship_turtle(this->width, this->height);

	this->body = geometry_freeze(turtle->snap_track());

	turtle->destroy();
}

void LightShiplet::resize(float width, float height) {
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
		this->construct_light_ship(true);
		this->notify_updated();
	}
}

ITurtle* LightShiplet::make_light_ship_turtle(float width, float height) {
	Turtle<S>* turtle = new Turtle<S>(width / ship_icon_base_size, height / ship_icon_base_size, true);

	turtle->reference();

	turtle->jump_right(ship_icon_base_size * 0.5F)->jump_down(4.0F, S::Home)->jump_down(0.5F);
	turtle->move_down(5.0F, S::Deck)->move_down(2.0F, S::Level)->move_right(2.0F);
	turtle->jump_right_down(0.5F)->move_down(1.0F, S::rLevel)->jump_left_down(0.5F)->move_left(4.0F);
	turtle->jump_left_up(0.5F, S::lLevel)->move_up(1.0F)->jump_right_up(0.5F)->move_to(S::Level);

	turtle->jump_back(S::Home)->jump_left(0.5F)->move_right_up(0.5F, S::l);
	turtle->move_right_up(1.5F)->jump_back()->move_left_up(2.0F)->jump_back();
	turtle->move_right_down(0.5F)->move_left_down(0.5F)->move_left_up(0.5F);
	turtle->move_left_down(1.0F);

	turtle->jump_back(S::lLevel)->move_left(ship_icon_base_size * 0.5F);
	turtle->jump_back(S::rLevel)->move_right(ship_icon_base_size * 0.5F);

	turtle->jump_back(S::Deck)->move_left(3.0F)->move_up(0.5F)->jump_left_up(0.5F)->move_left(1.5F, S::ll)->move_up(4.0F);
	turtle->jump_back()->move_left(2.0F)->jump_right(0.5F)->move_down(1.5F);
	turtle->jump_right_down(0.5F, S::ld)->move_right(2.5F)->jump_back()->move_down(2.0F);

	turtle->jump_back(S::Deck)->move_right(3.0F)->move_up(0.5F)->jump_right_up(0.5F)->move_right(1.5F, S::rl)->move_up(4.0F);
	turtle->jump_back()->move_right(2.0F)->jump_left(0.5F)->move_down(1.5F);
	turtle->jump_left_down(0.5F, S::ld)->move_left(2.5F)->jump_back()->move_down(2.0F);

	return turtle;
}

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

	ds->DrawEllipse(x + rx, y + ry, rx - half_thick, ry * 0.618F, this->ring_color, half_thick * 2.0F);
	ds->DrawCachedGeometry(this->body, x, y, this->body_color);
}

void SunkenShiplet::construct_sunken_ship(bool resized) {
	ITurtle* turtle = this->make_sunken_ship_turtle(this->width, this->height);

	this->body = geometry_freeze(turtle->snap_track(2.0F));

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
	Turtle<S>* turtle = new Turtle<S>(width / ship_icon_base_size, height / ship_icon_base_size, true);
	float quarter = ship_icon_base_size * 0.25F;

	turtle->reference();

	turtle->jump_down(ship_icon_base_size * 0.5F);
	turtle->move_right(quarter, S::ll)->move_right(quarter, S::l)->move_right(quarter, S::rl)->move_right(quarter);

	turtle->jump_back(S::ll)->move_up(2.0F)->jump_back(S::ll)->move_down(3.0F);
	turtle->jump_back(S::l)->move_up(3.0F)->jump_back(S::l)->move_down(4.0F);
	turtle->jump_back(S::rl)->move_up(2.0F)->jump_back(S::rl)->move_down(3.0F);

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
	Turtle<S>* turtle = new Turtle<S>(width / ship_icon_base_size, height / ship_icon_base_size, true);

	turtle->reference();

	turtle->jump_down(ship_icon_base_size * 0.5F)->jump_right(1.0F, S::Home);
	turtle->jump_right(4.0F)->jump_down(6.0F, S::ll)->move_to(S::Home)->jump_back();
	turtle->move_left(5.0F)->move_down(2.0F)->move_right(ship_icon_base_size)->move_up(2.0F);
	turtle->move_left(4.0F)->move_up(1.0F)->move_left(2.0F)->move_left_up(3.0F, S::l);
	turtle->move_left_down(1.0F)->move_to(S::Home)->jump_back();
	
	turtle->move_up(2.0F)->move_right(1.0F)->move_up(4.0F)->move_right(2.0F);
	turtle->move_down(4.0F)->move_left(1.0F)->move_down(4.0F)->move_to(S::l);

	return turtle;
}
