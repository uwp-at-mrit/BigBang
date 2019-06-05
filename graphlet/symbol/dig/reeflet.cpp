#include "graphlet/symbol/dig/reeflet.hpp"

#include "brushes.hxx"

using namespace WarGrey::SCADA;

using namespace Windows::Foundation;

using namespace Microsoft::Graphics::Canvas;
using namespace Microsoft::Graphics::Canvas::Brushes;

static const float reef_icon_base_size = 16.0F;

static CanvasSolidColorBrush^ default_tower_color = Colours::AntiqueWhite;

/*************************************************************************************************/
Reeflet::Reeflet(float size, ICanvasBrush^ color) : width(size), height(size), color((color == nullptr) ? default_tower_color : color) {
	this->enable_resizing(true);
}

void Reeflet::construct() {
	this->construct_reef(false);
}

void Reeflet::fill_extent(float x, float y, float* width, float* height) {
	SET_VALUES(width, this->width, height, this->height);
}

void Reeflet::draw(CanvasDrawingSession^ ds, float x, float y, float Width, float Height) {
	ds->DrawCachedGeometry(this->body, x, y, this->color);
}

void Reeflet::construct_reef(bool resized) {
	ITurtle* turtle = this->make_reef_turtle(this->width, this->height);

	this->body = geometry_freeze(turtle->snap_track());

	turtle->destroy();
}

void Reeflet::resize(float width, float height) {
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
		this->construct_reef(true);
		this->notify_updated();
	}
}

ITurtle* Reeflet::make_reef_turtle(float width, float height) {
	GreenTurtle* turtle = new GreenTurtle(width / reef_icon_base_size, height / reef_icon_base_size, true);

	turtle->reference();

	turtle->jump_right(reef_icon_base_size * 0.5F);
	turtle->move_down(1.0F, GreenTurtleAnchor::Home)->move_left(3.0F)->move_left_down(3.0F, 2.0F);
	turtle->move_down(1.0F)->move_left(1.0F)->move_down(1.0F);
	turtle->move_right_down(5.0F, 3.0F)->move_left_down(2.0F, 3.0F)->move_right_down(7.0F, 5.0F);
	turtle->move_right_up(4.0F, 8.0F)->move_left_up(4.0F, 5.0F)->move_left(2.0F);
	turtle->move_to(GreenTurtleAnchor::Home);

	return turtle;
}
