#include "graphlet/symbol/dig/buoylet.hpp"

#include "datum/file.hpp"
#include "datum/string.hpp"

#include "brushes.hxx"

using namespace WarGrey::SCADA;

using namespace Windows::Foundation;

using namespace Microsoft::Graphics::Canvas;
using namespace Microsoft::Graphics::Canvas::Brushes;
using namespace Microsoft::Graphics::Canvas::Geometry;

static const float buoy_base_size = 16.0F;

private enum class B {
	Home, Bottom,
	_,

	lb, rb
};

/*************************************************************************************************/
BuoyDig::BuoyDig(std::filebuf& dig, BuoyType subtype, float size)
	: IconDig(dig, DigDatumType::Buoy, size), subtype(subtype), size(size) {}

IGraphlet* BuoyDig::make_graphlet(double* x, double* y) {
	SET_VALUES(x, this->x, y, this->y);

	return new Buoylet(this->subtype, this->size);
}

Platform::String^ BuoyDig::to_string() {
	return make_wstring(L"%s%s(%f, %f)",
		this->type.ToString()->Data(), this->subtype.ToString()->Data(),
		this->x, this->y);
}

/*************************************************************************************************/
Buoylet::Buoylet(BuoyType subtype, float size) : IStatelet(subtype), width(size), height(size) {}

void Buoylet::construct_buoy(bool resized) {
	ITurtle* bturtle = nullptr;
	ITurtle* mturtle = nullptr;
	
	switch (this->get_state()) {
	case BuoyType::BlackYellow: case BuoyType::RedWhite: {
		bturtle = this->make_colored_buoy_turtle(this->width, this->height);
		mturtle = this->make_buoy_mask_turtle(this->width, this->height);
	}; break;
	case BuoyType::Green: case BuoyType::Red: case BuoyType::White: case BuoyType::Yellow: case BuoyType::Black: {
		bturtle = this->make_colored_buoy_turtle(this->width, this->height);
	}; break;
	case BuoyType::_1: bturtle = this->make_buoy_type1_turtle(this->width, this->height); break;
	case BuoyType::_2: bturtle = this->make_buoy_type2_turtle(this->width, this->height); break;
	case BuoyType::_3: bturtle = this->make_buoy_type3_turtle(this->width, this->height); break;
	default: bturtle = this->make_buoy_type4_turtle(this->width, this->height);
	}

	this->shape = bturtle->snap_path();

	if (mturtle != nullptr) {
		this->right_mask = mturtle->snap_path();
		mturtle->destroy();
	} else {
		this->right_mask = nullptr;
	}

	bturtle->destroy();
}

void Buoylet::fill_extent(float x, float y, float* width, float* height) {
	SET_VALUES(width, this->width, height, this->height);
}

void Buoylet::draw(CanvasDrawingSession^ ds, float x, float y, float Width, float Height) {
	BuoyStyle style = this->get_style();

	ds->DrawRectangle(x, y, Width, Height, Colours::Firebrick);

	if (style.ring_color != nullptr) {
		float rx = this->width * 0.5F;
		float ry = this->height * 0.5F;
		float half_thick = 1.0F;

		ds->DrawEllipse(x + rx, y + ry,
			rx - half_thick, ry - half_thick,
			style.ring_color, half_thick * 2.0F);
	}

	if (style.color != nullptr) {
		ds->FillGeometry(this->shape, x, y, style.color);

		if ((this->right_mask != nullptr) && (style.mask_color != nullptr)) {
			ds->FillGeometry(this->right_mask, x, y, style.mask_color);
		}
	}

	ds->DrawGeometry(this->shape, x, y, style.border_color);
}

bool Buoylet::resize(float width, float height) {
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
		this->construct_buoy(true);
	}

	return resized;
}

void Buoylet::prepare_style(BuoyType type, BuoyStyle& s) {
	switch (type) {
	case BuoyType::BlackYellow: CAS_VALUES(s.color, Colours::Khaki, s.mask_color, Colours::DimGray); break;
	case BuoyType::RedWhite: CAS_VALUES(s.color, Colours::Red, s.mask_color, Colours::Snow); break;
	case BuoyType::Green: CAS_SLOT(s.color, Colours::Green); break;
	case BuoyType::Red: CAS_SLOT(s.color, Colours::Red); break;
	case BuoyType::Yellow: CAS_SLOT(s.color, Colours::Yellow); break;
	case BuoyType::White: CAS_SLOT(s.color, Colours::GhostWhite); break;
	case BuoyType::Black: CAS_SLOT(s.color, Colours::DimGray); break;
	case BuoyType::_4: CAS_SLOT(s.ring_color, Colours::Tomato); break;
	}

	CAS_SLOT(s.border_color, Colours::Snow);

	// NOTE: The others can be nullptr;
}

void Buoylet::on_state_changed(BuoyType type) {
	this->construct_buoy(false);
}

ITurtle* Buoylet::make_colored_buoy_turtle(float width, float height) {
	GreenTurtle* buoy = new GreenTurtle(width / buoy_base_size, height / buoy_base_size, true);

	buoy->reference();

	buoy->jump_right(buoy_base_size * 0.618F, GreenTurtleAnchor::Home);
	buoy->move_down_left(2.0F)->move_down(3.0F)->move_left(1.0F)->move_down(2.0F)->move_left(1.0F)->move_down(2.0F)->move_left_down(6.0F);
	buoy->move_right(6.0F)->turn_up_right()->turn_right_down()->move_right(8.0F);
	buoy->move_left_up(4.0F)->move_up(3.0F)->move_left(1.0F)->move_up(5.0F)->move_left(1.0F)->move_up(1.0F)->move_to(GreenTurtleAnchor::Home);

	return buoy;
}

ITurtle* Buoylet::make_buoy_mask_turtle(float width, float height) {
	GreenTurtle* buoy = new GreenTurtle(width / buoy_base_size, height / buoy_base_size, true);

	buoy->reference();

	buoy->jump_right(buoy_base_size * 0.618F, GreenTurtleAnchor::Home);
	buoy->move_left(1.0F)->move_down(8.0F)->move_left(1.0F)->move_down(3.0F)->move_left(1.0F)->move_down(3.0F);
	buoy->turn_right_down()->move_right(8.0F);
	buoy->move_left_up(4.0F)->move_up(3.0F)->move_left(1.0F)->move_up(5.0F)->move_left(1.0F)->move_up(1.0F)->move_to(GreenTurtleAnchor::Home);

	return buoy;
}

ITurtle* Buoylet::make_buoy_type1_turtle(float width, float height) {
	Turtle<B>* buoy = new Turtle<B>(width / buoy_base_size * 2.0F, height / buoy_base_size * 2.0F, true);

	buoy->reference();

	buoy->jump_right(3.0F)->jump_down(1.5F, B::Home);
	buoy->jump_down(4.0F)->jump_left(2.0F, B::lb)->move_to(B::Home)->jump_back()->move_left(1.0F)->jump_right(1.0F);
	buoy->move_right(2.0F)->turn_up_right_down()->turn_down_left_up()->jump_right(2.0F)->move_right(3.0F);
	buoy->jump_left(1.5F, B::Bottom)->jump_up(2.5F)->jump_right(1.0F, B::rb)->move_to(B::Bottom)->jump_back()->move_to(B::Home);

	return buoy;
}

ITurtle* Buoylet::make_buoy_type2_turtle(float width, float height) {
	GreenTurtle* buoy = new GreenTurtle(width / buoy_base_size * 2.0F, height / buoy_base_size * 2.0F, true);

	buoy->reference();

	buoy->jump_right(5.0F)->jump_down(1.0F, GreenTurtleAnchor::Home);
	buoy->move_left(0.5F)->move_down_left(1.5F)->move_down(0.5F)->move_left(0.5F)->move_down(1.0F);
	buoy->move_left(0.5F)->move_down(1.0F)->move_left(0.5F)->move_down(1.0F)->move_left(1.5F)->jump_right(1.5F);
	buoy->move_right(1.5F)->turn_up_right_down()->turn_down_left_up()->jump_right(2.0F)->move_right(3.0F);
	buoy->jump_left(1.5F)->move_up(2.5F)->move_left(0.5F)->move_up(1.0F)->move_left(0.5F)->move_up(1.0F)->move_to(GreenTurtleAnchor::Home);

	return buoy;
}

ITurtle* Buoylet::make_buoy_type3_turtle(float width, float height) {
	GreenTurtle* buoy = new GreenTurtle(width / buoy_base_size * 2.0F, height / buoy_base_size * 2.0F, true);

	buoy->reference();

	buoy->jump_right(1.5F)->jump_down(2.0F, GreenTurtleAnchor::Home);
	buoy->move_down(3.0F)->move_left(1.5F)->jump_right(1.5F);
	buoy->move_right(1.5F)->turn_up_right_down()->turn_down_left_up()->jump_right(2.0F)->move_right(3.0F);
	buoy->jump_left(1.5F)->move_up(3.0F)->move_to(GreenTurtleAnchor::Home);

	return buoy;
}

ITurtle* Buoylet::make_buoy_type4_turtle(float width, float height) {
	Turtle<B>* buoy = new Turtle<B>(width / buoy_base_size * 2.0F, height / buoy_base_size * 2.0F, true);

	buoy->reference();

	buoy->jump_right(3.0F)->jump_down(1.5F, B::Home);
	buoy->jump_down(4.0F)->jump_left(2.0F, B::lb)->move_to(B::Home)->jump_back()->move_left(1.0F)->jump_right(1.0F);
	buoy->move_right(2.0F)->turn_up_right_down()->turn_down_left_up()->jump_right(2.0F)->move_right(3.0F);
	buoy->jump_left(1.5F, B::Bottom)->jump_up(2.5F)->jump_right(1.0F, B::rb)->move_to(B::Bottom)->jump_back()->move_to(B::Home);

	return buoy;
}

ITurtle* Buoylet::make_buoy_type5_turtle(float width, float height) {
	Turtle<B>* buoy = new Turtle<B>(width / buoy_base_size * 2.0F, height / buoy_base_size * 2.0F, true);

	buoy->reference();

	buoy->jump_right(3.0F)->jump_down(1.5F, B::Home);
	buoy->jump_down(4.0F)->jump_left(2.0F, B::lb)->move_to(B::Home)->jump_back()->move_left(1.0F)->jump_right(1.0F);
	buoy->move_right(2.0F)->turn_up_right_down()->turn_down_left_up()->jump_right(2.0F)->move_right(3.0F);
	buoy->jump_left(1.5F, B::Bottom)->jump_up(2.5F)->jump_right(1.0F, B::rb)->move_to(B::Bottom)->jump_back()->move_to(B::Home);

	return buoy;
}

ITurtle* Buoylet::make_buoy_type6_turtle(float width, float height) {
	Turtle<B>* buoy = new Turtle<B>(width / buoy_base_size * 2.0F, height / buoy_base_size * 2.0F, true);

	buoy->reference();

	buoy->jump_right(3.0F)->jump_down(1.5F, B::Home);
	buoy->jump_down(4.0F)->jump_left(2.0F, B::lb)->move_to(B::Home)->jump_back()->move_left(1.0F)->jump_right(1.0F);
	buoy->move_right(2.0F)->turn_up_right_down()->turn_down_left_up()->jump_right(2.0F)->move_right(3.0F);
	buoy->jump_left(1.5F, B::Bottom)->jump_up(2.5F)->jump_right(1.0F, B::rb)->move_to(B::Bottom)->jump_back()->move_to(B::Home);

	return buoy;
}

ITurtle* Buoylet::make_buoy_type7_turtle(float width, float height) {
	Turtle<B>* buoy = new Turtle<B>(width / buoy_base_size * 2.0F, height / buoy_base_size * 2.0F, true);

	buoy->reference();

	buoy->jump_right(3.0F)->jump_down(1.5F, B::Home);
	buoy->jump_down(4.0F)->jump_left(2.0F, B::lb)->move_to(B::Home)->jump_back()->move_left(1.0F)->jump_right(1.0F);
	buoy->move_right(2.0F)->turn_up_right_down()->turn_down_left_up()->jump_right(2.0F)->move_right(3.0F);
	buoy->jump_left(1.5F, B::Bottom)->jump_up(2.5F)->jump_right(1.0F, B::rb)->move_to(B::Bottom)->jump_back()->move_to(B::Home);

	return buoy;
}
