#include "graphlet/symbol/dig/buoylet.hpp"

#include "datum/file.hpp"
#include "datum/string.hpp"

using namespace WarGrey::SCADA;

using namespace Windows::Foundation;

using namespace Microsoft::Graphics::Canvas;
using namespace Microsoft::Graphics::Canvas::Brushes;

BuoyDig::BuoyDig(std::filebuf& dig, char subtype) : IconDig(dig, DigDatumType::Buoy), subtype(subtype) {}

Platform::String^ BuoyDig::to_string() {
	return make_wstring(L"%s%c(%f, %f)", this->type.ToString()->Data(), this->subtype, this->x, this->y);
}
