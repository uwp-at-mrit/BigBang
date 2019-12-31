#include "graphlet/symbol/dig/chimneylet.hpp"

#include "brushes.hxx"

using namespace WarGrey::SCADA;
using namespace WarGrey::DTPM;

using namespace Windows::Foundation;

using namespace Microsoft::Graphics::Canvas;
using namespace Microsoft::Graphics::Canvas::Brushes;

static const float chimney_icon_base_size = 16.0F;

static CanvasSolidColorBrush^ default_chimney_color = Colours::WhiteSmoke;

/*************************************************************************************************/
Chimneylet::Chimneylet(float size, ICanvasBrush^ color) : width(size), height(size), color((color == nullptr) ? default_chimney_color : color) {
	this->enable_resizing(true);
}

void Chimneylet::construct() {
	this->construct_chimney(false);
}

void Chimneylet::fill_extent(float x, float y, float* width, float* height) {
	SET_VALUES(width, this->width, height, this->height);
}

void Chimneylet::draw(CanvasDrawingSession^ ds, float x, float y, float Width, float Height) {
	ds->DrawCachedGeometry(this->body, x, y, this->color);
}

void Chimneylet::construct_chimney(bool resized) {
	ITurtle* turtle = this->make_chimney_turtle(this->width, this->height);

	this->body = geometry_freeze(turtle->snap_track());

	turtle->destroy();
}

void Chimneylet::resize(float width, float height) {
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
		this->construct_chimney(true);
		this->notify_updated();
	}
}

ITurtle* Chimneylet::make_chimney_turtle(float width, float height) {
	GreenTurtle* turtle = new GreenTurtle(width / chimney_icon_base_size, height / chimney_icon_base_size, true);

	turtle->reference();

	turtle->jump_right(chimney_icon_base_size * 0.5F)->jump_down(1.0F);
	turtle->move_right(2.0F)->move_right_down(1.0F)->move_right_up(1.0F)->move_right(2.0F)->move_right_down(1.0F);
	turtle->move_down(1.0F)->move_left_down(1.0F)->move_left(1.0F)->move_left_up(1.0F)->move_left(2.0F);

	turtle->jump_left(1.0F)->move_left(2.0F, GreenTurtleAnchor::Home)->move_left_up(1.0F)->move_right_up(1.0F)->move_right(2.0F);
	turtle->jump_back(GreenTurtleAnchor::Home)->jump_right_down(3.0F, 1.0F)->move_down(6.0F)->move_down_right(1.0F);
	turtle->move_down(4.0F)->move_right(3.0F)->jump_left(3.0F)->move_left(8.0F)->jump_right(3.0F);
	turtle->move_up(4.0F)->move_right_up(1.0F)->move_to(GreenTurtleAnchor::Home);

	return turtle;
}
