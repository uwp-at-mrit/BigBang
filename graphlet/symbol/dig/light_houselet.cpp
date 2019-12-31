#include "graphlet/symbol/dig/light_houselet.hpp"

#include "brushes.hxx"

using namespace WarGrey::SCADA;
using namespace WarGrey::DTPM;

using namespace Windows::Foundation;

using namespace Microsoft::Graphics::Canvas;
using namespace Microsoft::Graphics::Canvas::Brushes;

static const float lighthouse_icon_base_size = 16.0F;

static CanvasSolidColorBrush^ default_light_color = Colours::Crimson;
static CanvasSolidColorBrush^ default_house_color = Colours::Snow;

/*************************************************************************************************/
LightHouselet::LightHouselet(float size, ICanvasBrush^ light_color, ICanvasBrush^ house_color) : width(size), height(size)
, light_color((light_color == nullptr) ? default_light_color : light_color)
, house_color((house_color == nullptr) ? default_house_color : house_color) {
	this->enable_resizing(true);
}

void LightHouselet::construct() {
	this->construct_light_house(false);
}

void LightHouselet::fill_extent(float x, float y, float* width, float* height) {
	SET_VALUES(width, this->width, height, this->height);
}

void LightHouselet::draw(CanvasDrawingSession^ ds, float x, float y, float Width, float Height) {
	ds->DrawCachedGeometry(this->light, x + this->width * 0.0F, y + this->height * 0.0F, this->light_color);
	ds->DrawCachedGeometry(this->house, x + this->width * 0.5F, y + this->height * 0.5F, this->house_color);
}

void LightHouselet::construct_light_house(bool resized) {
	ITurtle* lturtle = this->make_light_turtle(this->width * 0.5F, this->height * 0.5F);
	ITurtle* hturtle = this->make_house_turtle(this->width * 0.5F, this->height * 0.5F);

	this->light = geometry_freeze(lturtle->snap_track());
	this->house = geometry_freeze(hturtle->snap_track());

	lturtle->destroy();
	hturtle->destroy();
}

void LightHouselet::resize(float width, float height) {
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
		this->construct_light_house(true);
		this->notify_updated();
	}
}

ITurtle* LightHouselet::make_light_turtle(float width, float height) {
	GreenTurtle* turtle = new GreenTurtle(width / lighthouse_icon_base_size, height / lighthouse_icon_base_size, true);

	turtle->reference();

	turtle->jump_down_right(4.0F, GreenTurtleAnchor::Home)->move_down_left(2.5F, 2.0F);
	turtle->move_right_down(3.0F)->move_right(2.0F)->move_right_down(4.0F)->move_right(2.0F)->move_down_right(4.0F)->move_right(2.0F);
	turtle->move_up(2.0F)->move_left_up(4.0F)->move_up(2.0F)->move_left_up(4.0F)->move_up(2.0F)->move_left_up(3.0F);
	turtle->move_to(GreenTurtleAnchor::Home);

	return turtle;
}


ITurtle* LightHouselet::make_house_turtle(float width, float height) {
	GreenTurtle* turtle = new GreenTurtle(width / lighthouse_icon_base_size, height / lighthouse_icon_base_size, true);

	turtle->reference();

	turtle->jump_right(lighthouse_icon_base_size * 0.5F)->jump_down(1.0F, GreenTurtleAnchor::Home);
	turtle->move_down_left(2.0F)->move_left(2.0F)->move_right_down(2.0F)->move_left_down(2.0F);
	turtle->move_right_up(4.0F, 1.0F)->move_right_down(4.0F, 1.0F);
	turtle->move_left_up(2.0F)->move_right_up(2.0F)->move_left(2.0F);
	turtle->move_to(GreenTurtleAnchor::Home);

	return turtle;
}
