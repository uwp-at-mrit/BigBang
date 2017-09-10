#include "object.hpp"
#include "pasteboard.hxx"
#include "orientation.hpp"

using namespace WarGrey::Win2DDemo;

using namespace Windows::UI::Xaml::Input;

struct LayoutInfo : public AbstractObject {
    float anchor;
};

VerticalLayout::VerticalLayout(float gap_size) : gapsize(gap_size) {};

bool VerticalLayout::can_move(Pasteboard^ self, PointerRoutedEventArgs^ e) {
    return false;
}

void VerticalLayout::before_insert(Pasteboard^ self, Snip* snip, float x, float y) {
    self->begin_edit_sequence();
};

void VerticalLayout::after_insert(Pasteboard^ self, Snip* snip, float x, float y) {
    float width = 0.0F;
    float height = 0.0F;

    if (self->layout_info == nullptr) {
        LayoutInfo* info = new LayoutInfo();
        info->anchor = -this->gapsize;
        self->layout_info = info;
    }

    LayoutInfo* info = (LayoutInfo*)self->layout_info;

    snip->fill_extent(&width, &height);
    self->move_to(snip, 0.0F, info->anchor + this->gapsize);
    info->anchor += (gapsize + height);
    self->end_edit_sequence();
};
