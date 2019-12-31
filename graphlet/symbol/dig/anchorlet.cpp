#include "graphlet/symbol/dig/anchorlet.hpp"

#include "brushes.hxx"
#include "paint.hpp"

using namespace WarGrey::SCADA;
using namespace WarGrey::DTPM;

using namespace Windows::Foundation;

using namespace Microsoft::Graphics::Canvas;
using namespace Microsoft::Graphics::Canvas::Brushes;

static const float anchor_icon_base_size = 16.0F;

static CanvasSolidColorBrush^ default_anchor_color = Colours::Silver;

namespace {
	private enum class A {
		Home,
		_,
		top, bottom
	};
}

/*************************************************************************************************/
Anchorlet::Anchorlet(float size, ICanvasBrush^ color) : width(size), height(size), color((color == nullptr) ? default_anchor_color : color) {
	this->enable_resizing(true);
}

void Anchorlet::construct() {
	this->construct_anchor(false);
}

void Anchorlet::fill_extent(float x, float y, float* width, float* height) {
	SET_VALUES(width, this->width, height, this->height);
}

void Anchorlet::draw(CanvasDrawingSession^ ds, float x, float y, float Width, float Height) {
	ds->DrawCachedGeometry(this->body, x, y, this->color);
}

void Anchorlet::construct_anchor(bool resized) {
	ITurtle* turtle = this->make_anchor_turtle(this->width, this->height);

	this->body = geometry_freeze(turtle->snap_track(2.0F, make_roundcap_stroke_style()));

	turtle->destroy();
}

void Anchorlet::resize(float width, float height) {
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
		this->construct_anchor(true);
		this->notify_updated();
	}
}

ITurtle* Anchorlet::make_anchor_turtle(float width, float height) {
	Turtle<A>* turtle = new Turtle<A>(width / anchor_icon_base_size * 2.0F, height / anchor_icon_base_size * 2.0F, true);

	turtle->reference();

	turtle->jump_right(anchor_icon_base_size * 0.25F)->jump_down(1.0F);
	turtle->turn_left_down_right(A::Home)->turn_right_up_left()->jump_back(A::Home);
	turtle->move_down(1.0F, A::top)->move_left(2.0F)->jump_back()->move_right(2.0F)->jump_back();
	turtle->move_down(4.0F, A::bottom)->move_left_up(3.0F, 2.0F)->jump_back()->move_right_up(3.0F, 2.0F);
		
	return turtle;
}
