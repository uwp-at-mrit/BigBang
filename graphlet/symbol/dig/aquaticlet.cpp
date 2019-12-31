#include "graphlet/symbol/dig/aquaticlet.hpp"

#include "brushes.hxx"
#include "paint.hpp"

using namespace WarGrey::SCADA;
using namespace WarGrey::DTPM;

using namespace Windows::Foundation;

using namespace Microsoft::Graphics::Canvas;
using namespace Microsoft::Graphics::Canvas::Brushes;

static const float aquatic_icon_base_size = 16.0F;

static CanvasSolidColorBrush^ default_aquatic_color = Colours::Azure;

/*************************************************************************************************/
Aquaticlet::Aquaticlet(float size, ICanvasBrush^ color) : width(size), height(size), color((color == nullptr) ? default_aquatic_color : color) {
	this->enable_resizing(true);
}

void Aquaticlet::construct() {
	this->construct_aquatic(false);
}

void Aquaticlet::fill_extent(float x, float y, float* width, float* height) {
	SET_VALUES(width, this->width, height, this->height);
}

void Aquaticlet::draw(CanvasDrawingSession^ ds, float x, float y, float Width, float Height) {
	ds->DrawCachedGeometry(this->body, x, y, this->color);
}

void Aquaticlet::construct_aquatic(bool resized) {
	ITurtle* turtle = this->make_aquatic_turtle(this->width, this->height);

	this->body = geometry_freeze(turtle->snap_track(2.0F, make_roundcap_stroke_style()));

	turtle->destroy();
}

void Aquaticlet::resize(float width, float height) {
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
		this->construct_aquatic(true);
		this->notify_updated();
	}
}

ITurtle* Aquaticlet::make_aquatic_turtle(float width, float height) {
	GreenTurtle* turtle = new GreenTurtle(width / aquatic_icon_base_size, height / aquatic_icon_base_size, true);
	
	turtle->reference();

	turtle->jump_down(2.0F)->move_right(aquatic_icon_base_size);
	turtle->jump_left(8.0F)->move_down(8.0F)->jump_right_up(6.0F, 3.0F)->move_left_up(4.0F, 5.0F);
	turtle->jump_left(4.0F)->move_left_down(5.0F, 10.0F);
		
	return turtle;
}
