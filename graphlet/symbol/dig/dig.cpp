#include "graphlet/symbol/dig/dig.hpp"

#include "datum/file.hpp"
#include "datum/string.hpp"

#include "graphlet/symbol/dig/sunken_shiplet.hpp"
#include "graphlet/symbol/dig/light_houselet.hpp"
#include "graphlet/symbol/dig/anchorlet.hpp"
#include "graphlet/symbol/dig/radar_reflectorlet.hpp"
#include "graphlet/symbol/dig/buoylet.hpp"

using namespace WarGrey::SCADA;

namespace {
	private struct DigDatum : public IDigDatum {
	public:
		DigDatum() : IDigDatum(DigDatumType::_) {}
	};
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

IconDig::IconDig(std::filebuf& dig, DigDatumType type) : IDigDatum(type) {
	this->x = read_flonum(dig);
	this->y = read_flonum(dig);
	this->name = read_wtext(dig);
}

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

		default: datum = new DigDatum();
		}
	}

	return datum;
}
