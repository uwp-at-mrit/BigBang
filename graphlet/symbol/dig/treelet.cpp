#include "graphlet/symbol/dig/treelet.hpp"

#include "brushes.hxx"

using namespace WarGrey::SCADA;

using namespace Windows::Foundation;

using namespace Microsoft::Graphics::Canvas;
using namespace Microsoft::Graphics::Canvas::Brushes;

static const float tree_icon_base_size = 16.0F;

static CanvasSolidColorBrush^ default_tower_color = Colours::SpringGreen;

/*************************************************************************************************/
Treelet::Treelet(float size, ICanvasBrush^ color) : width(size), height(size), color((color == nullptr) ? default_tower_color : color) {
	this->enable_resizing(true);
}

void Treelet::construct() {
	this->construct_tree(false);
}

void Treelet::fill_extent(float x, float y, float* width, float* height) {
	SET_VALUES(width, this->width, height, this->height);
}

void Treelet::draw(CanvasDrawingSession^ ds, float x, float y, float Width, float Height) {
	ds->DrawCachedGeometry(this->body, x, y, this->color);
}

void Treelet::construct_tree(bool resized) {
	ITurtle* turtle = this->make_tree_turtle(this->width, this->height);

	this->body = geometry_freeze(turtle->snap_track());

	turtle->destroy();
}

void Treelet::resize(float width, float height) {
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

ITurtle* Treelet::make_tree_turtle(float width, float height) {
	GreenTurtle* turtle = new GreenTurtle(width / tree_icon_base_size, height / tree_icon_base_size, true);

	turtle->reference();

	turtle->jump_right(tree_icon_base_size * 0.5F)->jump_down(1.0F);
	turtle->move_left_down(3.0F, 4.0F)->move_right(3.0F)->move_left_down(4.0F, 3.0F)->move_right(4.0F)->move_left_down(6.0F, 4.0F);
	turtle->move_right(5.0F)->move_down(3.0F)->move_left(3.0F)->jump_right(3.0F);
	turtle->move_right(5.0F)->jump_left(3.0F)->move_up(3.0F)->move_right(5.0F);
	turtle->move_left_up(6.0F, 4.0F)->move_right(4.0F)->move_left_up(4.0F, 3.0F)->move_right(3.0F)->move_left_up(3.0F, 4.0F);
		
	return turtle;
}
