#include <algorithm>

#include "absolute.hpp"
#include "universe.hpp"

using namespace WarGrey::SCADA;

AbsoluteLayout::AbsoluteLayout(float width, float height) : preferred_min_width(width), preferred_min_height(height) {}

void AbsoluteLayout::before_insert(Universe* master, Snip* snip, float x, float y) {};
void AbsoluteLayout::after_insert(Universe* master, Snip* snip, float x, float y) {};
