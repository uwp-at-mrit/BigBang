#include "layout/orientation.hxx"

using namespace WarGrey::Win2DDemo;

VerticalLayout::VerticalLayout(float gapsize) {
    this->gapsize = gapsize;
    this->anchor = -gapsize;
}

void VerticalLayout::before_insert(Pasteboard^ self, Snip* snip, float x, float y) {
    self->begin_edit_sequence();
};

void VerticalLayout::after_insert(Pasteboard^ self, Snip* snip, float x, float y) {
    float width = 0.0F;
    float height = 0.0F;

    snip->fill_extent(&width, &height);
    self->move(snip, 0.0F, anchor + gapsize);
    this->anchor += (gapsize + height);
    self->end_edit_sequence();
};
