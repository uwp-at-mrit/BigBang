#include "graphlet/symbol/dig/fishlet.hpp"

#include "brushes.hxx"

using namespace WarGrey::SCADA;
using namespace WarGrey::DTPM;

using namespace Windows::Foundation;

using namespace Microsoft::Graphics::Canvas;
using namespace Microsoft::Graphics::Canvas::Brushes;

static const float fish_icon_base_size = 16.0F;

static CanvasSolidColorBrush^ default_fish_color = Colours::Salmon;

/*************************************************************************************************/
Fishlet::Fishlet(float size, ICanvasBrush^ color) : width(size), height(size), color((color == nullptr) ? default_fish_color : color) {
	this->enable_resizing(true);
}

void Fishlet::construct() {
	this->construct_fish(false);
}

void Fishlet::fill_extent(float x, float y, float* width, float* height) {
	SET_VALUES(width, this->width, height, this->height);
}

void Fishlet::draw(CanvasDrawingSession^ ds, float x, float y, float Width, float Height) {
	ds->DrawCachedGeometry(this->body, x, y, this->color);
}

void Fishlet::construct_fish(bool resized) {
	ITurtle* turtle = this->make_fish_turtle(this->width, this->height);

	this->body = geometry_freeze(turtle->snap_track());

	turtle->destroy();
}

void Fishlet::resize(float width, float height) {
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
		this->construct_fish(true);
		this->notify_updated();
	}
}

ITurtle* Fishlet::make_fish_turtle(float width, float height) {
	GreenTurtle* turtle = new GreenTurtle(width / fish_icon_base_size, height / fish_icon_base_size, true);

	turtle->reference();

	turtle->jump_right(fish_icon_base_size)->jump_down(fish_icon_base_size * 0.5F);
	turtle->move_left(1.0F, GreenTurtleAnchor::Home);
	turtle->turn_up_left_down()->turn_down_right_up();
	turtle->drift(-14.0F, -3.0F, -4.0F, -8.0F, -10.0F, 1.0F);
	turtle->drift(2.0F, 3.0F, 1.0F, 3.0F)->drift(-2.0F, 4.0F, -1.0F, 1.0F);
	turtle->drift_to(GreenTurtleAnchor::Home, 4.0F, -4.0F, 10.0F, 2.0F);

	return turtle;
}
