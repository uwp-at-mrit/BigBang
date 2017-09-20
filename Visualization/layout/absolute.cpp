#include <algorithm>

#include "absolute.hpp"
#include "pasteboard.hxx"

using namespace WarGrey::SCADA;

AbsoluteLayout::AbsoluteLayout(float width, float height) : preferred_min_width(width), preferred_min_height(height) {}

void AbsoluteLayout::on_attach_to(Pasteboard^ master) {
    master->set_preferred_min_size(this->preferred_min_width, this->preferred_min_height);
    master->min_layer_width = std::max(this->preferred_min_width, master->min_canvas_width);
    master->min_layer_height = std::max(this->preferred_min_height, master->min_canvas_height);
}

void AbsoluteLayout::before_insert(Pasteboard^ master, Snip* snip, float x, float y) {};
void AbsoluteLayout::after_insert(Pasteboard^ master, Snip* snip, float x, float y) {};
