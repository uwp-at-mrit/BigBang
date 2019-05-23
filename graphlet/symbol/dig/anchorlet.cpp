#include "graphlet/symbol/dig/anchorlet.hpp"

#include "datum/file.hpp"

using namespace WarGrey::SCADA;

using namespace Windows::Foundation;

using namespace Microsoft::Graphics::Canvas;
using namespace Microsoft::Graphics::Canvas::Brushes;

AnchorDig::AnchorDig(std::filebuf& dig) : IDigDatum(DigDatumType::Anchor) {
	this->x = read_flonum(dig);
	this->y = read_flonum(dig);
}
