#include "graphlet/symbol/dig/buoylet.hpp"

#include "datum/file.hpp"
#include "datum/string.hpp"

#include "brushes.hxx"

using namespace WarGrey::SCADA;

using namespace Windows::Foundation;

using namespace Microsoft::Graphics::Canvas;
using namespace Microsoft::Graphics::Canvas::Brushes;
using namespace Microsoft::Graphics::Canvas::Geometry;

private enum class B {
	Start, Boundary,
	_
};

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
	ITurtle* tbuoy = this->make_colored_buoy_turtle(this->width, this->height);
	
	this->shape = tbuoy->snap_path();

	tbuoy->destroy();
}

void Buoylet::fill_extent(float x, float y, float* width, float* height) {
	SET_VALUES(width, this->width, height, this->height);
}

void Buoylet::draw(CanvasDrawingSession^ ds, float x, float y, float Width, float Height) {
	BuoyStyle style = this->get_style();

	ds->FillGeometry(this->shape, x, y, style.color);
	ds->DrawGeometry(this->shape, x, y, Colours::Background);
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
	case BuoyType::BlackYellow: {
		CAS_SLOT(s.color, Colours::Khaki);
	}; break;
	case BuoyType::Green: {
		CAS_SLOT(s.color, Colours::Green);
	}; break;
	case BuoyType::Red: {
		CAS_SLOT(s.color, Colours::Red);
	}; break;
	case BuoyType::_1: {
		CAS_SLOT(s.color, Colours::LightBlue);
	}; break;
	case BuoyType::_2: {
		CAS_SLOT(s.color, Colours::LightBlue);
	}; break;
	case BuoyType::_3: {
		CAS_SLOT(s.color, Colours::LightBlue);
	}; break;
	case BuoyType::_4: {
		CAS_SLOT(s.color, Colours::LightBlue);
	}; break;
	case BuoyType::_5: {
		CAS_SLOT(s.color, Colours::LightBlue);
	}; break;
	case BuoyType::_6: {
		CAS_SLOT(s.color, Colours::LightBlue);
	}; break;
	case BuoyType::_7: {
		CAS_SLOT(s.color, Colours::LightBlue);
	}; break;
	case BuoyType::_8: {
		CAS_SLOT(s.color, Colours::LightBlue);
	}; break;
	case BuoyType::_9: {
		CAS_SLOT(s.color, Colours::LightBlue);
	}; break;
	case BuoyType::_10: {
		CAS_SLOT(s.color, Colours::LightBlue);
	}; break;
	}

	CAS_SLOT(s.color, Colours::GhostWhite);

	// NOTE: The others can be nullptr;
}

void Buoylet::on_state_changed(BuoyType type) {
	this->construct_buoy(false);
}

ITurtle* Buoylet::make_colored_buoy_turtle(float width, float height) {
	float buoy_base_size = 16.0F;
	Turtle<B>* buoy = new Turtle<B>(width / buoy_base_size, height / buoy_base_size, true);

	buoy->reference();

	buoy->jump_right(buoy_base_size * 0.618F, B::Start);
	buoy->move_down_left(2.0F)->move_down(3.0F)->move_left(1.0F)->move_down(2.0F)->move_left(1.0F)->move_down(2.0F)->move_left_down(6.0F);
	buoy->move_right(7.0F)->turn_up_right_down()->move_right(8.0F, B::Boundary);
	buoy->move_left_up(4.0F)->move_up(3.0F)->move_left(1.0F)->move_up(5.0F)->move_left(1.0F)->move_up(1.0F)->move_to(B::Start);

	return buoy;
}
