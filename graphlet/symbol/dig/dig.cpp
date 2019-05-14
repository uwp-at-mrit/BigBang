#include "graphlet/symbol/dig/dig.hpp"

#include "datum/box.hpp"
#include "datum/file.hpp"
#include "datum/flonum.hpp"
#include "datum/string.hpp"

#include "graphlet/symbol/dig/sunken_shiplet.hpp"
#include "graphlet/symbol/dig/light_houselet.hpp"
#include "graphlet/symbol/dig/anchorlet.hpp"
#include "graphlet/symbol/dig/radar_reflectorlet.hpp"
#include "graphlet/symbol/dig/buoylet.hpp"

using namespace WarGrey::SCADA;

namespace {
	private struct _Dig : public IDigDatum {
	public:
		_Dig(char type) : IDigDatum(DigDatumType::_) {
			this->name = make_wstring(L"%c", type);
		}
	};
}

/*************************************************************************************************/
IconDig::IconDig(std::filebuf& dig, DigDatumType type) : IDigDatum(type) {
	this->y = read_flonum(dig);
	this->x = read_flonum(dig);
	this->name = read_wtext(dig, char_end_of_line);
}

void IDigDatum::fill_polar_extent(double* cx, double* cy, double* radius) {
	SET_VALUES(cx, this->x, cy, this->y);
	SET_BOX(radius, 0.0);
}

Platform::String^ IDigDatum::to_string() {
	Platform::String^ description = nullptr;

	if (this->name == nullptr) {
		description = make_wstring(L"%s(%f, %f)", this->type.ToString()->Data(), this->x, this->y);
	} else {
		description = make_wstring(L"%s[%s](%f, %f)", this->type.ToString()->Data(), this->name->Data(), this->x, this->y);
	}

	return description;
}

/*************************************************************************************************/
CompassDig::CompassDig(std::filebuf& dig) : IDigDatum(DigDatumType::Compass) {
	this->y = read_flonum(dig);
	this->x = read_flonum(dig);
	this->color = read_integer(dig);
}

Platform::String^ CompassDig::to_string() {
	return make_wstring(L"%s[%d](%f, %f)", this->type.ToString()->Data(), this->color, this->x, this->y);
}

/*************************************************************************************************/
ArcDig::ArcDig(std::filebuf& dig) : IDigDatum(DigDatumType::Arc) {
	this->y = read_flonum(dig);
	this->x = read_flonum(dig);
	this->start_degree = read_flonum(dig);
	this->stop_degree = read_flonum(dig);
	this->radius = read_flonum(dig);
	this->style = read_integer(dig);
	this->color = read_integer(dig);
}

void ArcDig::fill_polar_extent(double* cx, double* cy, double* radius) {
	IDigDatum::fill_polar_extent(cx, cy, nullptr);
	SET_BOX(radius, this->radius);
}

Platform::String^ ArcDig::to_string() {
	return make_wstring(L"%s[%d, %d](%f, %f, %f, %f, %f)", this->type.ToString()->Data(),
		this->style, this->color,
		this->x, this->y, this->start_degree, this->stop_degree, this->radius);
}

/*************************************************************************************************/
CircleDig::CircleDig(std::filebuf& dig) : IDigDatum(DigDatumType::Circle) {
	this->y = read_flonum(dig);
	this->x = read_flonum(dig);
	this->radius = read_flonum(dig);
	this->style = read_integer(dig);
	this->color = read_integer(dig);
	this->filled = read_integer(dig);
	this->fill_color = read_integer(dig);
}

void CircleDig::fill_polar_extent(double* cx, double* cy, double* radius) {
	IDigDatum::fill_polar_extent(cx, cy, nullptr);
	SET_BOX(radius, this->radius);
}

Platform::String^ CircleDig::to_string() {
	Platform::String^ attributes = nullptr;

	if (this->filled) {
		attributes = make_wstring(L"%d, %d, %d", this->style, this->color, this->fill_color);
	} else {
		attributes = make_wstring(L"%d, %d", this->style, this->color);
	}

	return make_wstring(L"%s[%s](%f, %f, %f)", this->type.ToString()->Data(), attributes->Data(), this->x, this->y, this->radius);
}

/*************************************************************************************************/
RectangleDig::RectangleDig(std::filebuf& dig) : IDigDatum(DigDatumType::Rectangle) {
	this->y = read_flonum(dig);
	this->x = read_flonum(dig);
	this->height = read_flonum(dig);
	this->width = read_flonum(dig);
	this->rotation = read_flonum(dig);
	this->style = read_integer(dig);
	this->color = read_integer(dig);
	this->pen_width = read_integer(dig);
}

void RectangleDig::fill_polar_extent(double* cx, double* cy, double* radius) {
	SET_BOX(cx, this->x + this->width * 0.5);
	SET_BOX(cy, this->y + this->height * 0.5);
	SET_BOX(radius, flmax(this->width, this->height) * 0.5);
}

Platform::String^ RectangleDig::to_string() {
	return make_wstring(L"%s[%d, %d, %d](%f, %f, %f, %f, %f)", this->type.ToString()->Data(),
		this->style, this->color, this->pen_width,
		this->x, this->y, this->width, this->height, this->rotation);
}

/*************************************************************************************************/
FontTextDig::FontTextDig(std::filebuf& dig) : IDigDatum(DigDatumType::FontText) {
	this->y = read_flonum(dig);
	this->x = read_flonum(dig);
	this->rotation = read_flonum(dig);
	this->color = read_integer(dig);
	this->font_size = read_integer(dig);
	this->style = read_integer(dig);
	this->width = read_integer(dig);
	this->name = read_wtext(dig, char_end_of_word);
	this->font_name = read_wtext(dig, char_end_of_line);
}

void FontTextDig::fill_polar_extent(double* cx, double* cy, double* radius) {
	//SET_BOX(cx, this->x + this->width * 0.5);
	//SET_BOX(cx, this->y + this->height * 0.5);
	//SET_BOX(radius, flmax(this->width, this->height) * 0.5);
	IDigDatum::fill_polar_extent(cx, cy, radius);
}

Platform::String^ FontTextDig::to_string() {
	Platform::String^ attributes = nullptr;

	if (this->font_name != nullptr) {
		attributes = make_wstring(L"%s, %d, %d, %d", this->font_name->Data(), this->color, this->font_size, this->style, this->width);
	} else {
		attributes = make_wstring(L"%d, %d, %d", this->color, this->font_size, this->style, this->width);
	}

	return make_wstring(L"%s[%s][%s](%f, %f, %f)", this->type.ToString()->Data(),
		this->name->Data(), attributes->Data(),
		this->color, this->font_size, this->style, this->width,
		this->x, this->y, this->width, this->rotation);
}

/*************************************************************************************************/
IDigDatum* WarGrey::SCADA::read_dig(std::filebuf& dig) {
	IDigDatum* datum = nullptr;
	char ch = read_char(dig);

	if (ch != EOF) {
		switch (ch) {
		case 'a': datum = new IconDig(dig, DigDatumType::SunkenShip); break;
		case 'b': datum = new IconDig(dig, DigDatumType::LightShip); break;
		case 'c': datum = new IconDig(dig, DigDatumType::OilWell); break;
		case 'd': datum = new IconDig(dig, DigDatumType::PilotStation); break;
		case 'e': datum = new IconDig(dig, DigDatumType::ReportPoint); break;
		case 'f': datum = new IconDig(dig, DigDatumType::LightHouse); break;
		case 'g': datum = new IconDig(dig, DigDatumType::RedFlag); break;
		case 'h': datum = new IconDig(dig, DigDatumType::Hoisptal); break;
		case 'i': datum = new IconDig(dig, DigDatumType::Tree); break;
		case 'j': datum = new IconDig(dig, DigDatumType::Anchor); break;
		case 'k': datum = new IconDig(dig, DigDatumType::Chimney); break;
		case 'l': datum = new IconDig(dig, DigDatumType::WaterTower); break;
		case 'm': datum = new IconDig(dig, DigDatumType::RadarReflector); break;
		case 'n': datum = new IconDig(dig, DigDatumType::IslandReef); break;
		case 'o': datum = new IconDig(dig, DigDatumType::Aquatic); break;

		case 'G': datum = new IconDig(dig, DigDatumType::TideStation); break;
		case 'K': datum = new IconDig(dig, DigDatumType::Kettle); break;
		case 'L': datum = new IconDig(dig, DigDatumType::Light); break;
		case 'N': datum = new IconDig(dig, DigDatumType::Seamark); break;
		case 'P': datum = new IconDig(dig, DigDatumType::Picket); break;
		case 'R': datum = new IconDig(dig, DigDatumType::Rock); break;
		case 'T': datum = new IconDig(dig, DigDatumType::Text); break;
		case 'U': datum = new IconDig(dig, DigDatumType::Number); break;
		case 'V': datum = new IconDig(dig, DigDatumType::FishNet); break;
		case 'W': datum = new IconDig(dig, DigDatumType::Wreck); break;

		case 'p': case '[': datum = new BuoyDig(dig, '1'); break;
		case 'q': datum = new BuoyDig(dig, '2'); break;
		case 'r': datum = new BuoyDig(dig, '3'); break;
		case 's': datum = new BuoyDig(dig, '4'); break;
		case 't': datum = new BuoyDig(dig, '5'); break;
		case 'u': datum = new BuoyDig(dig, '6'); break;
		case 'v': datum = new BuoyDig(dig, '7'); break;
		case 'w': datum = new BuoyDig(dig, '8'); break;
		case 'x': datum = new BuoyDig(dig, '9'); break;
		case 'y': datum = new BuoyDig(dig, 'a'); break;
		case 'B': datum = new BuoyDig(dig, 'W'); break;
		case 'M': datum = new BuoyDig(dig, 'G'); break;
		case 'F': datum = new BuoyDig(dig, 'R'); break;
		case 'Q': datum = new BuoyDig(dig, 'Y'); break;

		case 'A': datum = new ArcDig(dig); break;
		case 'C': datum = new CircleDig(dig); break;
		case 'J': datum = new CompassDig(dig); break;
		case 'O': datum = new RectangleDig(dig); break;
		case 'Z': datum = new FontTextDig(dig); break;

		default: datum = new _Dig(ch);
		}
	}

	return datum;
}
