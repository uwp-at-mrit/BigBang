#include <algorithm>
#include <WindowsNumerics.h>

#include "object.hpp"
#include "system.hpp"
#include "pasteboard.hxx"
#include "snip/snip.hpp"
#include "layout/absolute.hpp"
#include "decorator/decorator.hpp"

using namespace WarGrey::SCADA;

using namespace Windows::System;
using namespace Windows::Devices::Input;
using namespace Microsoft::Graphics::Canvas;
using namespace Microsoft::Graphics::Canvas::UI::Xaml;
using namespace Microsoft::Graphics::Canvas::Brushes;

using namespace Windows::UI;
using namespace Windows::UI::Input;
using namespace Windows::UI::Xaml;
using namespace Windows::UI::Xaml::Input;
using namespace Windows::UI::Xaml::Controls;
using namespace Windows::UI::ViewManagement;

using namespace Windows::Foundation;
using namespace Windows::Foundation::Numerics;

class PlaceHolderListener : public IPasteboardListener {};
class PlaceHolderDecorator : public IPasteboardDecorator {};

static Thickness default_padding(4.0, 4.0, 4.0, 4.0);

#define REMOVE(ptr, refcount) if (ptr->refcount <= 1) { delete ptr; } else { ptr->refcount -= 1; }

/*************************************************************************************************/
private ref class SnipInfo sealed : public WarGrey::SCADA::ISnipInfo {
public:
    SnipInfo(Pasteboard^ master) : _master(master) {};

    property Pasteboard^ master { virtual Pasteboard^ get() { return this->_master; }; }
    property CanvasDevice^ Device { virtual CanvasDevice^ get() { return this->_master->canvas->Device; }; }

internal:
    float x;
    float y;
    bool selected;

private:
    Pasteboard^ _master;
};

/** WARNING
 *   C-Style casting tries all C++ style casting except dynamic_cast;
 *   reinterpret_cast may cause "Access violation reading location 0xFFFFFFFFFFFFFFFF" even for subtype casting. 
 */

#define SNIP_INFO(snip) (static_cast<SnipInfo^>(snip->info))

static inline SnipInfo^ bind_snip_owership(Pasteboard^ master, Snip* snip) {
    auto info = ref new SnipInfo(master);
    snip->info = info;

    return info;
}

static inline void unsafe_move_snip_via_info(SnipInfo^ info, float x, float y, bool absolute) {
    if (!absolute) {
        x += info->x;
        y += info->y;
    }

    if ((info->x != x) || (info->y != y)) {
        info->x = x;
        info->y = y;

        info->master->size_cache_invalid();
        info->master->refresh();
    }
}

static inline void unsafe_add_selected(IPasteboardListener* listener, Snip* snip, SnipInfo^ info) {
    listener->before_select(info->master, snip);
    info->selected = true;
    info->master->refresh();
    listener->after_select(info->master, snip);
}

static inline void unsafe_set_selected(IPasteboardListener* listener, Snip* snip, SnipInfo^ info) {
    info->master->begin_edit_sequence();
    info->master->no_selected();
    unsafe_add_selected(listener, snip, info);
    info->master->end_edit_sequence();
}

/*************************************************************************************************/
Pasteboard::Pasteboard(Panel^ parent, IPasteboardLayout* layout) : Win2DCanvas(parent, nullptr) {
    this->padding = default_padding;
    this->layout = ((layout == nullptr) ? new AbsoluteLayout() : layout);
    this->layout->refcount += 1;
    this->layout->on_attach_to(this);

    this->control->PointerMoved += ref new PointerEventHandler(this, &Pasteboard::on_pointer_moved);
    this->control->PointerPressed += ref new PointerEventHandler(this, &Pasteboard::on_pointer_pressed);
    this->control->PointerReleased += ref new PointerEventHandler(this, &Pasteboard::on_pointer_released);

    this->listener = nullptr;
    this->set_pointer_listener(new PlaceHolderListener());
}

Pasteboard::~Pasteboard() {
    if (this->head_snip != nullptr) {
        Snip* child = nullptr;
        this->head_snip->prev->next = nullptr;
        do {
            child = this->head_snip;
            this->head_snip = this->head_snip->next;
            delete child; // snip's destructor will delete the associated info object
        } while (this->head_snip != nullptr);
    }

    if (this->layout_info != nullptr) {
        delete this->layout_info; // the layout object does not have to take care of the associated info object
    }

    REMOVE(this->layout, refcount);
    REMOVE(this->listener, refcount);
    REMOVE(this->decorator, refcount);
}

void Pasteboard::insert(Snip* snip, float x, float y) {
    if (snip->info == nullptr) { // TODO: should it be copied if one snip can only belongs to one pasteboard
        this->layout->before_insert(this, snip, x, y);

        if (this->head_snip == nullptr) {
            this->head_snip = snip;
            snip->prev = this->head_snip;
        } else {
            snip->prev = this->head_snip->prev;
            this->head_snip->prev->next = snip;
            this->head_snip->prev = snip;
        }
        snip->next = this->head_snip;

        auto info = bind_snip_owership(this, snip);
        this->begin_edit_sequence();
        unsafe_move_snip_via_info(info, x, y, true);
        this->size_cache_invalid();
        this->refresh();
        this->end_edit_sequence();
        this->layout->after_insert(this, snip, x, y);
    }
}

void Pasteboard::move_to(Snip* snip, float x, float y) {
    if ((snip != nullptr) && (snip->info != nullptr)) {
        if (snip->info->master == this) {
            SnipInfo^ info = SNIP_INFO(snip);
            unsafe_move_snip_via_info(info, x, y, true);
        }
    }
}

void Pasteboard::move(Snip* snip, float x, float y) {
    if ((snip != nullptr) && (snip->info != nullptr)) {
        if (snip->info->master == this) {
            SnipInfo^ info = SNIP_INFO(snip);
            unsafe_move_snip_via_info(info, x, y, false);
        }
    } else if (this->head_snip != nullptr) {
        Snip* child = this->head_snip;

        this->begin_edit_sequence();
        do {
            SnipInfo^ info = SNIP_INFO(child);
            if (info->selected) {
                unsafe_move_snip_via_info(info, x, y, false);
            }
            child = child->next;
        } while (child != this->head_snip);
        this->end_edit_sequence();
    }
}

Snip* Pasteboard::find_snip(float x, float y) {
    float width, height;
    Snip* found = nullptr;

    if (this->head_snip != nullptr) {
        Snip* child = this->head_snip;

        do {
            SnipInfo^ info = SNIP_INFO(child);
            child->fill_extent(info->x, info->y, &width, &height);

            if ((info->x < x) && (x < (info->x + width)) && (info->y < y) && (y < (info->y + height))) {
                found = child;
                break;
            }

            child = child->next;
        } while (child != this->head_snip);
    }
    
    return found;
}

bool Pasteboard::canvas_position_to_drawing_position(float* x, float* y) {
    bool xOK = true;
    bool yOK = true;

    if (x != nullptr) {
        (*x) -= float(this->padding.Left);
        xOK = ((*x) >= 0) && ((*x) <= this->layer_width);
    }

    if (y != nullptr) {
        (*y) -= float(this->padding.Top);
        yOK = ((*y) >= 0) && ((*y) <= this->layer_height);
    }

    return (xOK && yOK);
}

bool Pasteboard::drawing_position_to_canvas_position(float* x, float* y) {
    bool xOK = true;
    bool yOK = true;
    
    if (x != nullptr) {
        xOK = ((*x) >= 0) && ((*x) <= this->layer_width);
        (*x) += float(this->padding.Left);
    }

    if (y != nullptr) {
        yOK = ((*y) >= 0) && ((*y) <= this->layer_height);
        (*y) += float(this->padding.Top);
    }

    return (xOK && yOK);
}

void Pasteboard::set_preferred_min_size(float min_width, float min_height) {
    this->preferred_min_width = std::max(min_width, 0.0F);
    this->preferred_min_height = std::max(min_height, 0.0F);
}

void Pasteboard::fill_snips_bounds(float* x, float* y, float* width, float* height) {
    this->recalculate_snips_extent_when_invalid();
    if (x != nullptr) (*x) = this->snips_left;
    if (y != nullptr) (*y) = this->snips_top; 
    if (width != nullptr) (*width) = this->snips_right - this->snips_left;
    if (height != nullptr) (*height) = this->snips_bottom - this->snips_top;
}

void Pasteboard::size_cache_invalid() {
    this->snips_right = this->snips_left - 1.0F;
}

void Pasteboard::recalculate_snips_extent_when_invalid() {
    if (this->snips_right < this->snips_left) {
        float width, height;

        if (this->head_snip == nullptr) {
            this->snips_left = 0.0F;
            this->snips_top = 0.0F;
            this->snips_right = 0.0F;
            this->snips_bottom = 0.0F;
        } else {
            Snip* child = this->head_snip;

            this->snips_left = FLT_MAX;
            this->snips_top = FLT_MAX;
            this->snips_right = -FLT_MAX;
            this->snips_bottom = -FLT_MAX;
            
            do {
                SnipInfo^ info = SNIP_INFO(child);
                child->fill_extent(info->x, info->y, &width, &height);
                this->snips_left = std::min(this->snips_left, info->x);
                this->snips_top = std::min(this->snips_top, info->y);
                this->snips_right = std::max(this->snips_right, info->x + width);
                this->snips_bottom = std::max(this->snips_bottom, info->y + height);

                child = child->next;
            } while (child != this->head_snip);
        }

        this->min_layer_width = std::max(this->snips_right, this->preferred_min_width);
        this->min_layer_height = std::max(this->snips_bottom, this->preferred_min_height);
    }
}

void Pasteboard::on_end_edit_sequence() {
    this->recalculate_snips_extent_when_invalid();
}

void Pasteboard::add_selected(Snip* snip) {
    if (snip != nullptr) {
        if ((snip->info != nullptr) && (snip->info->master == this)) {
            SnipInfo^ info = SNIP_INFO(snip);
            if ((!info->selected) && (this->rubberband_allowed && this->listener->can_select(this, snip))) {
                unsafe_add_selected(this->listener, snip, info);
            }
        }
    }
}

void Pasteboard::set_selected(Snip* snip) {
    if (snip != nullptr) {
        if ((snip->info != nullptr) && (snip->info->master == this)) {
            SnipInfo^ info = SNIP_INFO(snip);
            if ((!info->selected) && (this->listener->can_select(this, snip))) {
                unsafe_set_selected(this->listener, snip, info);
            }
        }
    }
}

void Pasteboard::no_selected() {
    if (this->head_snip != nullptr) {
        Snip* child = this->head_snip;

        this->begin_edit_sequence();
        do {
            SnipInfo^ info = SNIP_INFO(child);
            if (info->selected) {
                this->listener->before_deselect(this, child);
                info->selected = false;
                this->refresh();
                this->listener->after_deselect(this, child);
            }

            child = child->next;
        } while (child != this->head_snip);
        this->end_edit_sequence();
    }
}

/************************************************************************************************/
void Pasteboard::on_pointer_moved(Object^ sender, PointerRoutedEventArgs^ e) {
    if (!e->Handled) {
        auto ppt = e->GetCurrentPoint(this->control);
        float x = ppt->Position.X;
        float y = ppt->Position.Y;

        if (ppt->Properties->IsLeftButtonPressed) {
            this->canvas_position_to_drawing_position(&x, &y);
            if (this->rubberband_y == nullptr) {
                if (layout->can_move(this, e)) {
                    this->move(nullptr, x - this->last_pointer_x, y - this->last_pointer_y);
                    this->last_pointer_x = x;
                    this->last_pointer_y = y;
                }
            } else {
                (*this->rubberband_x) = x;
                (*this->rubberband_y) = y;
                this->refresh();
            }
        }

        e->Handled = true;
    }
}

void Pasteboard::on_pointer_pressed(Object^ sender, PointerRoutedEventArgs^ e) {
    if ((!e->Handled) && (this->control->CapturePointer(e->Pointer))) {
        auto ppt = e->GetCurrentPoint(this->control);
        float x = ppt->Position.X;
        float y = ppt->Position.Y;
        
        if (ppt->Properties->IsLeftButtonPressed) {
            if (this->canvas_position_to_drawing_position(&x, &y)) {
                Snip* snip = this->find_snip(x, y);

                this->last_pointer_x = x;
                this->last_pointer_y = y;

                if (snip == nullptr) {
                    this->rubberband_y = this->rubberband_allowed ? (this->rubberband_x + 1) : nullptr;
                    this->no_selected();
                } else {
                    this->rubberband_y = nullptr;
                    SnipInfo^ info = SNIP_INFO(snip);
                    if ((!info->selected) && this->listener->can_select(this, snip)) {
                        if (e->KeyModifiers == VirtualKeyModifiers::Shift) {
                            if (this->rubberband_allowed) {
                                unsafe_add_selected(this->listener, snip, info);
                            }
                        } else {
                            unsafe_set_selected(this->listener, snip, info);
                        }
                    }
                }
            } else {
                this->no_selected();
            }
        }

        e->Handled = true;
    }
}

void Pasteboard::on_pointer_released(Object^ sender, PointerRoutedEventArgs^ e) {
    if (!e->Handled) {
        if (this->rubberband_y != nullptr) {
            this->rubberband_y = nullptr;
            this->refresh();
        }

        e->Handled = true;
    }
}

/*************************************************************************************************/
void Pasteboard::inset::set(Thickness v) { this->padding = v; }
Thickness Pasteboard::inset::get() { return this->padding; }

float Pasteboard::actual_layer_width::get() { return float(this->canvas->ActualWidth - this->inset.Left - this->inset.Right); }
float Pasteboard::actual_layer_height::get() { return float(this->canvas->ActualHeight - this->inset.Top - this->inset.Bottom); }

void Pasteboard::max_layer_width::set(float v) { this->canvas->MaxWidth = double(v) + this->inset.Left + this->inset.Right; }
float Pasteboard::max_layer_width::get() { return float(this->canvas->MaxWidth - this->inset.Left - this->inset.Right); }

void Pasteboard::max_layer_height::set(float v) { this->canvas->MaxHeight = double(v) + this->inset.Top + this->inset.Bottom; }
float Pasteboard::max_layer_height::get() { return float(this->canvas->MaxHeight - this->inset.Top - this->inset.Bottom); }

void Pasteboard::min_layer_width::set(float v) { this->canvas->MinWidth = double(v) + this->inset.Left + this->inset.Right; }
float Pasteboard::min_layer_width::get() { return float(this->canvas->MinWidth - this->inset.Left - this->inset.Right); }

void Pasteboard::min_layer_height::set(float v) { this->canvas->MinHeight = double(v) + this->inset.Top + this->inset.Bottom; }
float Pasteboard::min_layer_height::get() { return float(this->canvas->MinHeight - this->inset.Top - this->inset.Bottom); }

void Pasteboard::layer_width::set(float v) { this->canvas->Height = double(v) + this->inset.Top + this->inset.Bottom; }
float Pasteboard::layer_width::get() { return float(this->canvas->ActualWidth - this->inset.Left - this->inset.Right); }

void Pasteboard::layer_height::set(float v) { this->canvas->Height = double(v) + this->inset.Top + this->inset.Bottom; }
float Pasteboard::layer_height::get() { return float(this->canvas->ActualHeight - this->inset.Top - this->inset.Bottom); }

void Pasteboard::set_pointer_listener(IPasteboardListener* listener) {
    if (this->listener != nullptr) {
        REMOVE(this->listener, refcount);
    }

    this->listener = (listener == nullptr) ? new PlaceHolderListener() : listener;
    this->listener->refcount += 1;
    this->rubberband_allowed = this->listener->can_select_multiple(this);
}

void Pasteboard::set_decorator(IPasteboardDecorator* decorator) {
    if (this->decorator != nullptr) {
        REMOVE(this->decorator, refcount);
    }

    this->decorator = (decorator == nullptr) ? new PlaceHolderDecorator() : decorator;
    this->decorator->refcount += 1;
    this->refresh();
}

void Pasteboard::show_selection_dots(bool show) {
    if (this->draw_selection_dots != show) {
        this->draw_selection_dots = show;
        this->refresh();
    }
}

/*************************************************************************************************/
void Pasteboard::draw(CanvasDrawingSession^ ds) {
    float Width = this->actual_layer_width;
    float Height = this->actual_layer_height;
    float tx = (float)this->inset.Left;
    float ty = (float)this->inset.Top;
    float width, height;
    Rect border(-tx, -ty, this->canvas_width, this->actual_height);

    // https://blogs.msdn.microsoft.com/win2d/2014/09/15/why-does-win2d-include-three-different-sets-of-vector-and-matrix-types/
    ds->Transform = make_float3x2_translation(tx, ty);
    this->decorator->draw_before(this, ds, Width, Height, border);

    auto region = ds->CreateLayer(1.0F, Rect(0.0F, 0.0F, Width, Height));

    if (this->head_snip != nullptr) {
        Snip* child = this->head_snip;

        do {
            SnipInfo^ info = SNIP_INFO(child);
            child->fill_extent(info->x, info->y, &width, &height);
            width = std::min(Width - info->x, width);
            height = std::min(Height - info->y, height);

            if ((info->x < Width) && (info->y < Height) && ((info->x + width) > 0) && ((info->y + height) > 0)) {
                auto layer = ds->CreateLayer(1.0F, Rect(info->x, info->y, width, height));
                child->draw(ds, info->x, info->y, width, height);
                if ((info->selected) && (this->draw_selection_dots)) {
                    ds->FillCircle(info->x + width / 2.0F, info->y + height / 2.0F, 4.0F, Colors::White);
                }
                delete layer; /* Must Close the Layer Explicitly */
            }

            child = child->next;
        } while (child != this->head_snip);
    }

    if (this->rubberband_y != nullptr) {
        static auto rubberband_color = ref new CanvasSolidColorBrush(ds, system_color(UIElementType::Highlight));

        float left = std::min(this->last_pointer_x, (*this->rubberband_x));
        float top = std::min(this->last_pointer_y, (*this->rubberband_y));
        float width = std::abs((*this->rubberband_x) - this->last_pointer_x);
        float height = std::abs((*this->rubberband_y) - this->last_pointer_y);
        
        rubberband_color->Opacity = 0.32F;
        ds->FillRectangle(left, top, width, height, rubberband_color);
        rubberband_color->Opacity = 1.00F;
        ds->DrawRectangle(left, top, width, height, rubberband_color);
    }

    delete region; /* Must Close the Layer Explicitly */

    this->decorator->draw_after(this, ds, Width, Height, border);
}
