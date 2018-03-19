#include "iconlet.hpp"

#include "paint.hpp"

using namespace WarGrey::SCADA;

void Iconlet::fill_extent(float x, float y, float* w, float* h) {
	SET_BOXES(w, h, this->size);
}
