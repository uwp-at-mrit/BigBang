#include "graphlet/symbol/dig/sunken_shiplet.hpp"

#include "datum/file.hpp"

using namespace WarGrey::SCADA;

using namespace Windows::Foundation;

using namespace Microsoft::Graphics::Canvas;
using namespace Microsoft::Graphics::Canvas::Brushes;

SunkenShipDig::SunkenShipDig(std::filebuf& dig) : IDigDatum(DigDatumType::SunkenShip) {
	this->x = read_flonum(dig);
	this->y = read_flonum(dig);
}
