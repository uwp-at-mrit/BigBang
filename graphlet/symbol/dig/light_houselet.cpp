#include "graphlet/symbol/dig/light_houselet.hpp"

#include "datum/file.hpp"

using namespace WarGrey::SCADA;

using namespace Windows::Foundation;

using namespace Microsoft::Graphics::Canvas;
using namespace Microsoft::Graphics::Canvas::Brushes;

LightHouseDig::LightHouseDig(std::filebuf& dig) : IDigDatum(DigDatumType::LightHouse) {
	this->x = read_flonum(dig);
	this->y = read_flonum(dig);
	this->name = read_wtext(dig);
}
