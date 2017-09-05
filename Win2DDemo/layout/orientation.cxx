#include "orientation.hxx"

using namespace WarGrey::Win2DDemo;

VerticalLayout::VerticalLayout(float gap_size) : gapsize(gap_size) {};

void VerticalLayout::before_insert(Pasteboard^ self, Snip* snip, float x, float y) {
    self->begin_edit_sequence();
};

void VerticalLayout::after_insert(Pasteboard^ self, Snip* snip, float x, float y) {
    float width = 0.0F;
    float height = 0.0F;

    if (self->layout_info == nullptr) {
        self->layout_info = new float(-this->gapsize);
    }
    float* anchor = (float*)self->layout_info;

    snip->fill_extent(&width, &height);
    self->move(snip, 0.0F, (*anchor) + this->gapsize);
    (*anchor) += (gapsize + height);
    self->end_edit_sequence();
};
