#include "graphlet/symbol/dig/welllet.hpp"

#include "brushes.hxx"

using namespace WarGrey::SCADA;

using namespace Windows::Foundation;

using namespace Microsoft::Graphics::Canvas;
using namespace Microsoft::Graphics::Canvas::Brushes;

static const float well_icon_base_size = 16.0F;

static CanvasSolidColorBrush^ default_rig_color = Colours::RoyalBlue;
static CanvasSolidColorBrush^ default_well_color = Colours::Snow;

namespace {
	private enum class W {
		Home,
		_,
		t, r, b, l
	};
}

/*************************************************************************************************/
OilWelllet::OilWelllet(float size, ICanvasBrush^ light_color, ICanvasBrush^ house_color) : width(size), height(size)
, rig_color((rig_color == nullptr) ? default_rig_color : light_color)
, well_color((well_color == nullptr) ? default_well_color : house_color) {
	this->enable_resizing(true);
}

void OilWelllet::construct() {
	this->construct_light_house(false);
}

void OilWelllet::fill_extent(float x, float y, float* width, float* height) {
	SET_VALUES(width, this->width, height, this->height);
}

void OilWelllet::draw(CanvasDrawingSession^ ds, float x, float y, float Width, float Height) {
	ds->DrawCachedGeometry(this->well, x + this->width * 0.5F, y + this->height * 0.5F, this->well_color);
	ds->DrawCachedGeometry(this->rig, x + this->width * 0.0F, y + this->height * 0.0F, this->rig_color);
}

void OilWelllet::construct_light_house(bool resized) {
	ITurtle* rturtle = this->make_rig_turtle(this->width * 0.5F, this->height * 0.5F);
	ITurtle* wturtle = this->make_well_turtle(this->width * 0.5F, this->height * 0.5F);

	this->rig = geometry_freeze(rturtle->snap_track());
	this->well = geometry_freeze(wturtle->snap_track());

	rturtle->destroy();
	wturtle->destroy();
}

void OilWelllet::resize(float width, float height) {
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

ITurtle* OilWelllet::make_rig_turtle(float width, float height) {
	GreenTurtle* turtle = new GreenTurtle(width / well_icon_base_size, height / well_icon_base_size, true);

	turtle->reference();

	turtle->jump_down_right(4.0F, GreenTurtleAnchor::Home)->move_down_left(2.5F, 2.0F);
	turtle->move_right_down(7.0F)->move_right(2.0F)->move_right_down(2.0F)->move_right(2.0F)->move_down_right(2.0F)->move_right(2.0F);
	turtle->move_up(2.0F)->move_left_up(2.0F)->move_up(2.0F)->move_left_up(2.0F)->move_up(2.0F)->move_left_up(7.0F);
	turtle->move_to(GreenTurtleAnchor::Home);

	return turtle;
}


ITurtle* OilWelllet::make_well_turtle(float width, float height) {
	Turtle<W>* turtle = new Turtle<W>(width / well_icon_base_size, height / well_icon_base_size, true, W::Home);

	turtle->reference();

	turtle->move_down(4.0F, W::l)->move_down(4.0F)->move_right(4.0F, W::b)->move_right(4.0F);
	turtle->move_up(4.0F, W::r)->move_up(4.0F)->move_left(4.0F, W::t)->move_to(W::Home);

	turtle->jump_back(W::t)->move_down(2.0F);
	turtle->jump_back(W::r)->move_left(2.0F);
	turtle->jump_back(W::b)->move_up(2.0F);
	turtle->jump_back(W::l)->move_right(2.0F);

	return turtle;
}
