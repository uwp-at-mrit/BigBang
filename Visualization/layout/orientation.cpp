#include "object.hpp"
#include "pasteboard.hxx"
#include "orientation.hpp"
#include "snip/snip.hpp"

using namespace WarGrey::SCADA;

using namespace Windows::UI::Xaml::Input;

struct LayoutInfo : public AbstractObject {
    float anchor;
};

/*************************************************************************************************/
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

    LayoutInfo* info = static_cast<LayoutInfo*>(self->layout_info);
    
    y = info->anchor + this->gapsize;
    snip->fill_extent(0.0F, y, &width, &height);
    self->move_to(snip, 0.0F, y);
    info->anchor += (gapsize + height);
    self->end_edit_sequence();
};

/*************************************************************************************************/
HorizontalLayout::HorizontalLayout(float gap_size) : gapsize(gap_size) {};

bool HorizontalLayout::can_move(Pasteboard^ self, PointerRoutedEventArgs^ e) {
    return false;
}

void HorizontalLayout::before_insert(Pasteboard^ self, Snip* snip, float x, float y) {
    self->begin_edit_sequence();
};

void HorizontalLayout::after_insert(Pasteboard^ self, Snip* snip, float x, float y) {
    float width = 0.0F;
    float height = 0.0F;

    if (self->layout_info == nullptr) {
        LayoutInfo* info = new LayoutInfo();
        info->anchor = -this->gapsize;
        self->layout_info = info;
    }

    LayoutInfo* info = static_cast<LayoutInfo*>(self->layout_info);

    x = info->anchor + this->gapsize;
    snip->fill_extent(x, 0.0F, &width, &height);
    self->move_to(snip, x, 0.0F);
    info->anchor += (gapsize + height);
    self->end_edit_sequence();
};
