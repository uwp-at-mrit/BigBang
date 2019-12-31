#include "graphlet/symbol/dig/flaglet.hpp"

#include "brushes.hxx"

using namespace WarGrey::SCADA;
using namespace WarGrey::DTPM;

using namespace Windows::Foundation;

using namespace Microsoft::Graphics::Canvas;
using namespace Microsoft::Graphics::Canvas::Brushes;

static const float flag_icon_base_size = 16.0F;

static CanvasSolidColorBrush^ default_flag_color = Colours::Crimson;

/*************************************************************************************************/
Flaglet::Flaglet(float size, ICanvasBrush^ color) : width(size), height(size), color((color == nullptr) ? default_flag_color : color) {
	this->enable_resizing(true);
}

void Flaglet::construct() {
	this->construct_tree(false);
}

void Flaglet::fill_extent(float x, float y, float* width, float* height) {
	SET_VALUES(width, this->width, height, this->height);
}

void Flaglet::draw(CanvasDrawingSession^ ds, float x, float y, float Width, float Height) {
	ds->DrawCachedGeometry(this->body, x, y, this->color);
}

void Flaglet::construct_tree(bool resized) {
	ITurtle* turtle = this->make_tree_turtle(this->width, this->height);

	this->body = geometry_freeze(turtle->snap_path());

	turtle->destroy();
}

void Flaglet::resize(float width, float height) {
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
		this->construct_tree(true);
		this->notify_updated();
	}
}

ITurtle* Flaglet::make_tree_turtle(float width, float height) {
	GreenTurtle* turtle = new GreenTurtle(width / flag_icon_base_size, height / flag_icon_base_size, true);

	turtle->reference();

	turtle->jump_right(flag_icon_base_size * 0.5F)->jump_down(2.0F, GreenTurtleAnchor::Home);
	turtle->turn_up_left_down()->move_down(12.0F)->move_left(4.0F)->turn_left_down_right()->move_right(10.0F)->turn_right_up_left();
	turtle->move_left(4.0F)->move_up(7.0F)->move_right(5.0F)->move_left_up(2.0F, 3.0F)->move_left(3.0F);
	turtle->move_to(GreenTurtleAnchor::Home);
		
	return turtle;
}
