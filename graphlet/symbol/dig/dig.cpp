#include "graphlet/symbol/dig/dig.hpp"

#include "datum/box.hpp"
#include "datum/file.hpp"
#include "datum/flonum.hpp"
#include "datum/string.hpp"

#include "graphlet/shapelet.hpp"
#include "graphlet/symbol/dig/shiplet.hpp"
#include "graphlet/symbol/dig/light_houselet.hpp"
#include "graphlet/symbol/dig/anchorlet.hpp"
#include "graphlet/symbol/dig/radar_reflectorlet.hpp"
#include "graphlet/symbol/dig/buoylet.hpp"
#include "graphlet/symbol/dig/welllet.hpp"
#include "graphlet/symbol/dig/report_spotlet.hpp"
#include "graphlet/symbol/dig/pilot_stationlet.hpp"
#include "graphlet/symbol/dig/water_towerlet.hpp"
#include "graphlet/symbol/dig/chimneylet.hpp"
#include "graphlet/symbol/dig/treelet.hpp"
#include "graphlet/symbol/dig/aquaticlet.hpp"
#include "graphlet/symbol/dig/reeflet.hpp"
#include "graphlet/symbol/dig/flaglet.hpp"
#include "graphlet/symbol/dig/hospitallet.hpp"
#include "graphlet/symbol/dig/lightlet.hpp"
#include "graphlet/symbol/dig/fishlet.hpp"
#include "graphlet/symbol/dig/navigation_marklet.hpp"
#include "graphlet/symbol/dig/picketlet.hpp"

using namespace WarGrey::SCADA;

namespace {
	private struct _Dig : public IDigDatum {
	public:
		_Dig(char type) : IDigDatum(DigDatumType::_) {
			this->name = make_wstring(L"%c", type);
		}
	};
}

static IGraphlet* create_icon_graphlet(DigDatumType type, float size) {
	IGraphlet* icon = nullptr;

	switch (type) {
	case DigDatumType::LightShip: icon = new LightShiplet(size); break;
	case DigDatumType::SunkenShip: icon = new SunkenShiplet(size); break;
	case DigDatumType::Wreck: icon = new Wrecklet(size); break;
	case DigDatumType::Anchor: icon = new Anchorlet(size); break;
	case DigDatumType::RadarReflector: icon = new RadarReflectorlet(size); break;
	case DigDatumType::LightHouse: icon = new LightHouselet(size); break;
	case DigDatumType::OilWell: icon = new OilWelllet(size); break;
	case DigDatumType::ReportSpot: icon = new ReportSpotlet(size); break;
	case DigDatumType::PilotStation: icon = new PilotStationlet(size); break;
	case DigDatumType::WaterTower: icon = new WaterTowerlet(size); break;
	case DigDatumType::Chimney: icon = new Chimneylet(size); break;
	case DigDatumType::Tree: icon = new Treelet(size); break;
	case DigDatumType::Aquatic: icon = new Aquaticlet(size); break;
	case DigDatumType::IslandReef: icon = new Reeflet(size); break;
	case DigDatumType::RedFlag: icon = new Flaglet(size); break;
	case DigDatumType::Hoisptal: icon = new Hospitallet(size); break;
	case DigDatumType::Light: icon = new Lightlet(size); break;
	case DigDatumType::FishingFloat: icon = new Fishlet(size); break;
	case DigDatumType::NavigationMark: icon = new NavigationMarklet(size); break;
	case DigDatumType::Picket: icon = new Picketlet(size); break;
	default: icon = new Rectanglet(size, Colours::Azure);
	}

	return icon;
}

/*************************************************************************************************/
IDigDatum* WarGrey::SCADA::read_dig(std::filebuf& dig, float icon_size) {
	IDigDatum* datum = nullptr;
	char ch = read_char(dig);

	if (ch != EOF) {
		switch (ch) {
		case 'a': datum = new IconDig(dig, DigDatumType::LightShip, icon_size); break;
		case 'b': datum = new IconDig(dig, DigDatumType::SunkenShip, icon_size); break;
		case 'c': datum = new IconDig(dig, DigDatumType::OilWell, icon_size); break;
		case 'd': datum = new IconDig(dig, DigDatumType::PilotStation, icon_size); break;
		case 'e': datum = new IconDig(dig, DigDatumType::ReportSpot, icon_size); break;
		case 'f': datum = new IconDig(dig, DigDatumType::LightHouse, icon_size); break;
		case 'g': datum = new IconDig(dig, DigDatumType::RedFlag, icon_size); break;
		case 'h': datum = new IconDig(dig, DigDatumType::Hoisptal, icon_size); break;
		case 'i': datum = new IconDig(dig, DigDatumType::Tree, icon_size); break;
		case 'j': datum = new IconDig(dig, DigDatumType::Anchor, icon_size); break;
		case 'k': datum = new IconDig(dig, DigDatumType::Chimney, icon_size); break;
		case 'l': datum = new IconDig(dig, DigDatumType::WaterTower, icon_size); break;
		case 'm': datum = new IconDig(dig, DigDatumType::RadarReflector, icon_size); break;
		case 'n': datum = new IconDig(dig, DigDatumType::IslandReef, icon_size); break;
		case 'o': datum = new IconDig(dig, DigDatumType::Aquatic, icon_size); break;

		case 'G': datum = new IconDig(dig, DigDatumType::TideStation, icon_size); break;
		case 'K': datum = new IconDig(dig, DigDatumType::Kettle, icon_size); break;
		case 'L': datum = new IconDig(dig, DigDatumType::Light, icon_size); break;
		case 'N': datum = new IconDig(dig, DigDatumType::NavigationMark, icon_size); break;
		case 'P': datum = new IconDig(dig, DigDatumType::Picket, icon_size); break;
		case 'R': datum = new IconDig(dig, DigDatumType::Rock, icon_size); break;
		case 'V': datum = new IconDig(dig, DigDatumType::FishingFloat, icon_size); break;
		case 'W': datum = new IconDig(dig, DigDatumType::Wreck, icon_size); break;

		case 'p': datum = new BuoyDig(dig, BuoyType::_1, icon_size); break;
		case 'q': datum = new BuoyDig(dig, BuoyType::_2, icon_size); break;
		case 'r': datum = new BuoyDig(dig, BuoyType::_3, icon_size); break;
		case 's': datum = new BuoyDig(dig, BuoyType::_4, icon_size); break;
		case 't': datum = new BuoyDig(dig, BuoyType::_5, icon_size); break;
		case 'u': datum = new BuoyDig(dig, BuoyType::_6, icon_size); break;
		case 'v': datum = new BuoyDig(dig, BuoyType::_7, icon_size); break;
		case 'w': datum = new BuoyDig(dig, BuoyType::RedWhite, icon_size); break;
		case 'x': datum = new BuoyDig(dig, BuoyType::Yellow, icon_size); break;
		case 'y': datum = new BuoyDig(dig, BuoyType::Black, icon_size); break;
		case 'B': datum = new BuoyDig(dig, BuoyType::White, icon_size); break;
		case 'M': datum = new BuoyDig(dig, BuoyType::Green, icon_size); break;
		case 'F': datum = new BuoyDig(dig, BuoyType::Red, icon_size); break;
		case 'Q': datum = new BuoyDig(dig, BuoyType::BlackYellow, icon_size); break;

		case 'T': datum = new IconDig(dig, DigDatumType::Text, icon_size); break;
		case 'U': datum = new IconDig(dig, DigDatumType::Number, icon_size); break;

		case 'A': datum = new ArcDig(dig); break;
		case 'C': datum = new CircleDig(dig); break;
		case 'H': datum = new TyphoonDig(dig); break;
		case 'I': datum = new PolylineDig(dig); break;
		case 'J': datum = new CompassDig(dig); break;
		case 'O': datum = new RectangleDig(dig); break;
		case 'X': datum = new LineDig(dig); break;
		case 'Z': datum = new FontTextDig(dig); break;

		default: datum = new _Dig(ch);
		}

		{ // read multilines
			IMultilineDigDatum* mdatum = dynamic_cast<IMultilineDigDatum*>(datum);

			if (mdatum != nullptr) {
				discard_this_line(dig);

				while ((ch = read_char(dig)) != EOF) {
					if (ch == 'E') {
						break;
					}

					mdatum->append_line(dig);
					discard_this_line(dig);
				}
			}
		}
	}

	return datum;
}

/*************************************************************************************************/
IGraphlet* IDigDatum::make_graphlet(double* x, double* y) {
	SET_VALUES(x, this->x, y, this->y);

	return nullptr;
}

void IDigDatum::fill_enclosing_box(double* x, double* y, double* width, double* height) {
	SET_VALUES(x, this->x, y, this->y);
	SET_BOXES(width, height, 0.0);
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

IconDig::IconDig(std::filebuf& dig, DigDatumType type, float size) : IDigDatum(type), size(size) {
	this->y = read_flonum(dig);
	this->x = read_flonum(dig);
	this->name = read_wtext(dig, char_end_of_line);
}

IGraphlet* IconDig::make_graphlet(double* x, double* y) {
	SET_VALUES(x, this->x, y, this->y);

	return create_icon_graphlet(this->type, this->size);
}

void IconDig::fill_enclosing_box(double* x, double* y, double* width, double* height) {
	SET_VALUES(x, this->x, y, this->y);
	SET_BOXES(width, height, this->size);
}

void IMultilineDigDatum::append_line(std::filebuf& dig) {
	this->rest_ys.push_back(read_flonum(dig));
	this->rest_xs.push_back(read_flonum(dig));
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

void ArcDig::fill_enclosing_box(double* x, double* y, double* width, double* height) {
	// TODO: compute the real enclosing box
	SET_VALUES(x, this->x - this->radius, y, this->y - this->radius);
	SET_BOXES(width, height, this->radius * 2.0);
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

void CircleDig::fill_enclosing_box(double* x, double* y, double* width, double* height) {
	SET_VALUES(x, this->x - this->radius, y, this->y - this->radius);
	SET_BOXES(width, height, this->radius * 2.0);
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

void RectangleDig::fill_enclosing_box(double* x, double* y, double* width, double* height) {
	// TODO: compute the real enclosing box
	SET_VALUES(x, this->x, y, this->y);
	SET_VALUES(width, this->width, height, this->height);
}

Platform::String^ RectangleDig::to_string() {
	return make_wstring(L"%s[%d, %d, %d](%f, %f, %f, %f, %f)", this->type.ToString()->Data(),
		this->style, this->color, this->pen_width,
		this->x, this->y, this->width, this->height, this->rotation);
}

/*************************************************************************************************/
LineDig::LineDig(std::filebuf& dig) : IDigDatum(DigDatumType::Line) {
	this->y = read_flonum(dig);
	this->x = read_flonum(dig);
	this->stop_x = read_flonum(dig);
	this->stop_y = read_flonum(dig);
	this->style = read_integer(dig);
	this->color = read_integer(dig);
	this->linewidth = read_integer(dig);
}

void LineDig::fill_enclosing_box(double* x, double* y, double* width, double* height) {
	SET_BOX(x, flmin(this->x, this->stop_x));
	SET_BOX(y, flmin(this->y, this->stop_y));
	SET_BOX(width, flabs(this->stop_x - this->x));
	SET_BOX(height, flabs(this->stop_y - this->y));
}

Platform::String^ LineDig::to_string() {
	return make_wstring(L"%s[%d, %d, %d](%f, %f, %f, %f)", this->type.ToString()->Data(),
		this->style, this->color, this->linewidth,
		this->x, this->y, this->stop_x, this->stop_y);
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

void FontTextDig::fill_enclosing_box(double* x, double* y, double* width, double* height) {
	// TODO: compute the real enclosing box
	SET_VALUES(x, this->x, y, this->y);
	SET_VALUES(width, 1.0, height, 1.0);
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
TyphoonDig::TyphoonDig(std::filebuf& dig) : IMultilineDigDatum(DigDatumType::Typhoon) {
	this->style = read_integer(dig);
	this->linewidth = read_integer(dig);
	this->color = read_integer(dig);
	this->radius = read_integer(dig);
	this->name = read_wtext(dig, char_end_of_line);
}

void TyphoonDig::append_line(std::filebuf& dig) {
	this->datetimes.push_back(read_wtext(dig, char_end_of_word));
	this->rest_xs.push_back(read_flonum(dig));
	this->rest_ys.push_back(read_flonum(dig));
	this->max_wind_speeds.push_back(read_flonum(dig));
	this->pressures.push_back(read_flonum(dig));
	this->move_speeds.push_back(read_flonum(dig));

	if (this->rest_xs.size() == 1) {
		this->x = this->rest_xs[0];
		this->y = this->rest_ys[0];
	}
}

void TyphoonDig::fill_enclosing_box(double* x, double* y, double* width, double* height) {
	// TODO: compute the real enclosing box
	SET_VALUES(x, this->x, y, this->y);
	SET_VALUES(width, 1.0, height, 1.0);
}

Platform::String^ TyphoonDig::to_string() {
	return make_wstring(L"%s[%s][%d, %d, %d, %d](%f, %f){%d}", this->type.ToString()->Data(),
		this->name->Data(),
		this->color, this->linewidth, this->color, this->radius,
		this->x, this->y, this->datetimes.size());
}

/*************************************************************************************************/
PolylineDig::PolylineDig(std::filebuf& dig) : IMultilineDigDatum(DigDatumType::Polyline) {
	this->y = read_flonum(dig);
	this->x = read_flonum(dig);
	this->color = read_integer(dig);
	this->style = read_integer(dig);
	this->subtype = read_integer(dig);
	this->width = read_integer(dig);
}

void PolylineDig::fill_enclosing_box(double* x, double* y, double* width, double* height) {
	// TODO: compute the real enclosing box
	SET_VALUES(x, this->x, y, this->y);
	SET_VALUES(width, 1.0, height, 1.0);
}

Platform::String^ PolylineDig::to_string() {
	return make_wstring(L"%s[%d, %d, %d, %d](%f, %f){+%d}", this->type.ToString()->Data(),
		this->color, this->style, this->subtype, this->width,
		this->x, this->y, this->rest_xs.size());
}
