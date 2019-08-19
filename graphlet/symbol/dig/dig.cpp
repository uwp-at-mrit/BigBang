#include "graphlet/symbol/dig/dig.hpp"

#include "datum/box.hpp"
#include "datum/file.hpp"
#include "datum/flonum.hpp"
#include "datum/string.hpp"

#include "math.hpp"
#include "text.hpp"

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
#include "graphlet/symbol/dig/kettlet.hpp"
#include "graphlet/symbol/dig/tide_stationlet.hpp"
#include "graphlet/symbol/dig/rocklet.hpp"

using namespace WarGrey::SCADA;

using namespace Windows::Foundation;
using namespace Windows::Foundation::Numerics;

using namespace Microsoft::Graphics::Canvas::Geometry;

/*************************************************************************************************/
namespace {
	private struct _Dig : public IDigDatum {
	public:
		_Dig(char type) : IDigDatum(DigDatumType::_) {
			this->name = make_wstring(L"%c", type);
		}
	};
}

static IGraphlet* create_icon_graphlet(DigIcon type, float size) {
	IGraphlet* icon = nullptr;

	switch (type) {
	case DigIcon::LightShip: icon = new LightShiplet(size); break;
	case DigIcon::SunkenShip: icon = new SunkenShiplet(size); break;
	case DigIcon::Wreck: icon = new Wrecklet(size); break;
	case DigIcon::Anchor: icon = new Anchorlet(size); break;
	case DigIcon::RadarReflector: icon = new RadarReflectorlet(size); break;
	case DigIcon::LightHouse: icon = new LightHouselet(size); break;
	case DigIcon::OilWell: icon = new OilWelllet(size); break;
	case DigIcon::ReportSpot: icon = new ReportSpotlet(size); break;
	case DigIcon::PilotStation: icon = new PilotStationlet(size); break;
	case DigIcon::WaterTower: icon = new WaterTowerlet(size); break;
	case DigIcon::Chimney: icon = new Chimneylet(size); break;
	case DigIcon::Tree: icon = new Treelet(size); break;
	case DigIcon::Aquatic: icon = new Aquaticlet(size); break;
	case DigIcon::IslandReef: icon = new Reeflet(size); break;
	case DigIcon::RedFlag: icon = new Flaglet(size); break;
	case DigIcon::Hoisptal: icon = new Hospitallet(size); break;
	case DigIcon::Light: icon = new Lightlet(size); break;
	case DigIcon::FishingFloat: icon = new Fishlet(size); break;
	case DigIcon::NavigationMark: icon = new NavigationMarklet(size); break;
	case DigIcon::Picket: icon = new Picketlet(size); break;
	case DigIcon::Kettle: icon = new Kettlet(size); break;
	case DigIcon::TideStation: icon = new TideStationlet(size); break;
	case DigIcon::Rock: icon = new Rocklet(size); break;
	default: icon = new Rectanglet(size, Colours::Azure);
	}

	return icon;
}

static void rotate_endpoints(double* lx, double* ty, double* rx, double* by, double rotation) {
	if (degrees_normalize(rotation) != 0.0) {
		CanvasGeometry^ g = rotate_rectangle(float(*lx), float(*ty), float(*rx - *lx), float(*by - *ty), rotation, float(*lx), float(*ty));
		Rect box = g->ComputeBounds();

		(*lx) = box.X;
		(*ty) = box.Y;
		(*rx) = box.X + box.Width;
		(*by) = box.Y + box.Height;
	}
}

/*************************************************************************************************/
IDigDatum* WarGrey::SCADA::read_dig_line(std::filebuf& dig, float icon_size) {
	IDigDatum* datum = nullptr;
	char ch = read_char(dig);

	if (ch != EOF) {
		switch (ch) {
		case 'a': datum = new IconDig(dig, DigIcon::LightShip, icon_size); break;
		case 'b': datum = new IconDig(dig, DigIcon::SunkenShip, icon_size); break;
		case 'c': datum = new IconDig(dig, DigIcon::OilWell, icon_size); break;
		case 'd': datum = new IconDig(dig, DigIcon::PilotStation, icon_size); break;
		case 'e': datum = new IconDig(dig, DigIcon::ReportSpot, icon_size); break;
		case 'f': datum = new IconDig(dig, DigIcon::LightHouse, icon_size); break;
		case 'g': datum = new IconDig(dig, DigIcon::RedFlag, icon_size); break;
		case 'h': datum = new IconDig(dig, DigIcon::Hoisptal, icon_size); break;
		case 'i': datum = new IconDig(dig, DigIcon::Tree, icon_size); break;
		case 'j': datum = new IconDig(dig, DigIcon::Anchor, icon_size); break;
		case 'k': datum = new IconDig(dig, DigIcon::Chimney, icon_size); break;
		case 'l': datum = new IconDig(dig, DigIcon::WaterTower, icon_size); break;
		case 'm': datum = new IconDig(dig, DigIcon::RadarReflector, icon_size); break;
		case 'n': datum = new IconDig(dig, DigIcon::IslandReef, icon_size); break;
		case 'o': datum = new IconDig(dig, DigIcon::Aquatic, icon_size); break;

		case 'G': datum = new IconDig(dig, DigIcon::TideStation, icon_size); break;
		case 'K': datum = new IconDig(dig, DigIcon::Kettle, icon_size); break;
		case 'L': datum = new IconDig(dig, DigIcon::Light, icon_size); break;
		case 'N': datum = new IconDig(dig, DigIcon::NavigationMark, icon_size); break;
		case 'P': datum = new IconDig(dig, DigIcon::Picket, icon_size); break;
		case 'R': datum = new IconDig(dig, DigIcon::Rock, icon_size); break;
		case 'V': datum = new IconDig(dig, DigIcon::FishingFloat, icon_size); break;
		case 'W': datum = new IconDig(dig, DigIcon::Wreck, icon_size); break;

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

		case 'T': datum = new IconDig(dig, DigIcon::Text, icon_size); break;
		case 'U': datum = new IconDig(dig, DigIcon::Number, icon_size); break;

		case 'A': datum = new ArcDig(dig); break;
		case 'C': datum = new CircleDig(dig); break;
		case 'D': datum = new PolyBezierDig(dig); break;
		case 'H': datum = new TyphoonDig(dig); break;
		case 'I': datum = new PolylineDig(dig); break;
		case 'J': datum = new CompassDig(dig); break;
		case 'O': datum = new RectangleDig(dig); break;
		case 'X': datum = new LineDig(dig); break;
		case 'Z': datum = new FontTextDig(dig); break;

		default: datum = new _Dig(ch);
		}

		if (datum->multiline()) {
			discard_this_line(dig);

			while ((ch = read_char(dig)) != EOF) {
				if (ch == 'E') {
					break;
				}

				datum->push_line(dig);
				discard_this_line(dig);
			}
		}

		discard_this_line(dig);
	}

	return datum;
}

/*************************************************************************************************/
IGraphlet* IDigDatum::make_graphlet(double* x, double* y) {
	SET_VALUES(x, this->x, y, this->y);

	return nullptr;
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

IconDig::IconDig(std::filebuf& dig, DigIcon type, float size) : IDigDatum(DigDatumType::Icon), subtype(type), size(size) {
	this->y = read_flonum(dig);
	this->x = read_flonum(dig);
	this->name = read_wtext(dig, char_end_of_line);

	this->lx = this->x;
	this->ty = this->y;
	this->rx = this->x + size;
	this->by = this->y + size;
}

IGraphlet* IconDig::make_graphlet(double* x, double* y) {
	SET_VALUES(x, this->x, y, this->y);

	return create_icon_graphlet(this->subtype, this->size);
}

bool IMultilineDigDatum::multiline() {
	return true;
}

void IMultilineDigDatum::push_line(std::filebuf& dig) {
	double y = read_flonum(dig);
	double x = read_flonum(dig);

	this->poly_ys.push_back(y);
	this->poly_xs.push_back(x);

	this->lx = flmin(this->lx, x);
	this->rx = flmax(this->rx, x);
	this->ty = flmin(this->ty, y);
	this->by = flmax(this->by, y);

	// NOTE: the rest parameters are duplicates of the head-line 
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

	// TODO: compute the real enclosing box
	this->lx = this->x - this->radius;
	this->ty = this->y - this->radius;
	this->rx = this->x + this->radius;
	this->by = this->y + this->radius;
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

	this->lx = this->x - this->radius;
	this->ty = this->y - this->radius;
	this->rx = this->x + this->radius;
	this->by = this->y + this->radius;
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

	this->lx = this->x;
	this->ty = this->y;
	this->rx = this->x + this->width;
	this->by = this->y + this->height;

	rotate_endpoints(&this->lx, &this->ty, &this->rx, &this->by, this->rotation);
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

	if (this->x < this->stop_x) {
		this->lx = this->x;
		this->rx = this->stop_x;
	} else {
		this->lx = this->stop_x;
		this->rx = this->x;
	}

	if (this->y < this->stop_y) {
		this->ty = this->y;
		this->by = this->stop_y;
	} else {
		this->ty = this->stop_y;
		this->by = this->y;
	}
}

Platform::String^ LineDig::to_string() {
	return make_wstring(L"%s[%d, %d, %d](%f, %f, %f, %f)", this->type.ToString()->Data(),
		this->style, this->color, this->linewidth,
		this->x, this->y, this->stop_x, this->stop_y);
}

/*************************************************************************************************/
PolyBezierDig::PolyBezierDig(std::filebuf& dig) : IMultilineDigDatum(DigDatumType::PolyBezier) {
	this->y = read_flonum(dig);
	this->x = read_flonum(dig);
	this->color = read_integer(dig);
	this->style = read_integer(dig);
	this->line_width = read_integer(dig);

	this->lx = this->x;
	this->ty = this->y;
	this->rx = this->x;
	this->by = this->y;
}

Platform::String^ PolyBezierDig::to_string() {
	return make_wstring(L"%s[%d, %d, %d](%f, %f){+%d}", this->type.ToString()->Data(),
		this->color, this->style, this->line_width,
		this->x, this->y, this->poly_xs.size());
}

/*************************************************************************************************/
PolylineDig::PolylineDig(std::filebuf& dig) : IMultilineDigDatum(DigDatumType::Polyline) {
	this->y = read_flonum(dig);
	this->x = read_flonum(dig);
	this->color = read_integer(dig);
	this->style = read_integer(dig);
	this->subtype = read_integer(dig);
	this->width = read_integer(dig);

	// TODO: compute the enclosing box endpoints
}

Platform::String^ PolylineDig::to_string() {
	return make_wstring(L"%s[%d, %d, %d, %d](%f, %f){+%d}", this->type.ToString()->Data(),
		this->color, this->style, this->subtype, this->width,
		this->x, this->y, this->poly_xs.size());
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

	{ // compute enclosing box endpoints
		TextExtent te = get_text_extent(this->name, make_text_format(this->font_name), this->font_size);

		this->lx = this->x;
		this->ty = this->y;
		this->rx = this->lx + te.width;
		this->by = this->by + te.height;

		rotate_endpoints(&this->lx, &this->ty, &this->rx, &this->by, this->rotation);
	}
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
CompassDig::CompassDig(std::filebuf& dig) : IDigDatum(DigDatumType::Compass) {
	this->y = read_flonum(dig);
	this->x = read_flonum(dig);
	this->color = read_integer(dig);
}

Platform::String^ CompassDig::to_string() {
	return make_wstring(L"%s[%d](%f, %f)", this->type.ToString()->Data(), this->color, this->x, this->y);
}

TyphoonDig::TyphoonDig(std::filebuf& dig) : IMultilineDigDatum(DigDatumType::Typhoon) {
	this->style = read_integer(dig);
	this->linewidth = read_integer(dig);
	this->color = read_integer(dig);
	this->radius = read_integer(dig);
	this->name = read_wtext(dig, char_end_of_line);
}

void TyphoonDig::push_line(std::filebuf& dig) {
	this->datetimes.push_back(read_wtext(dig, char_end_of_word));
	this->poly_xs.push_back(read_flonum(dig));
	this->poly_ys.push_back(read_flonum(dig));
	this->max_wind_speeds.push_back(read_flonum(dig));
	this->pressures.push_back(read_flonum(dig));
	this->move_speeds.push_back(read_flonum(dig));

	if (this->poly_xs.size() == 1) {
		this->x = this->poly_xs[0];
		this->y = this->poly_ys[0];
	}

	// TODO: compute the enclosing box endpoints
}

Platform::String^ TyphoonDig::to_string() {
	return make_wstring(L"%s[%s][%d, %d, %d, %d](%f, %f){%d}", this->type.ToString()->Data(),
		this->name->Data(),
		this->color, this->linewidth, this->color, this->radius,
		this->x, this->y, this->datetimes.size());
}
