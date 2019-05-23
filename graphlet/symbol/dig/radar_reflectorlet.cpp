#include "graphlet/symbol/dig/radar_reflectorlet.hpp"

#include "datum/file.hpp"

using namespace WarGrey::SCADA;

using namespace Windows::Foundation;

using namespace Microsoft::Graphics::Canvas;
using namespace Microsoft::Graphics::Canvas::Brushes;

RadarReflectorDig::RadarReflectorDig(std::filebuf& dig) : IDigDatum(DigDatumType::RadarReflector) {
	this->x = read_flonum(dig);
	this->y = read_flonum(dig);
}
