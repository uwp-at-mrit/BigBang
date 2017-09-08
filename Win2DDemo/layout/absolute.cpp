#include <algorithm>

#include "absolute.hpp"

using namespace std;
using namespace WarGrey::Win2DDemo;

using namespace Windows::UI::Xaml::Input;

AbsoluteLayout::AbsoluteLayout(float width, float height) : preferred_min_width(width), preferred_min_height(height) {}

void AbsoluteLayout::on_attach_to(Pasteboard^ self) {
    self->set_preferred_min_size(this->preferred_min_width, this->preferred_min_height);
    self->min_layer_width = max(this->preferred_min_width, self->min_canvas_width);
    self->min_layer_height = max(this->preferred_min_height, self->min_canvas_height);
}

void AbsoluteLayout::before_insert(Pasteboard^ self, Snip* snip, float x, float y) {};
void AbsoluteLayout::after_insert(Pasteboard^ self, Snip* snip, float x, float y) {};
