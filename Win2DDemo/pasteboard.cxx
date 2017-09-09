#include <algorithm>
#include <WindowsNumerics.h>

#include "object.hpp"
#include "pasteboard.hxx"
#include "layout/absolute.hpp"

using namespace WarGrey::Win2DDemo;

using namespace Windows::System;
using namespace Windows::Devices::Input;
using namespace Microsoft::Graphics::Canvas;
using namespace Microsoft::Graphics::Canvas::Brushes;
using namespace Microsoft::Graphics::Canvas::Geometry;

using namespace Windows::UI;
using namespace Windows::UI::Input;
using namespace Windows::UI::Xaml;
using namespace Windows::UI::Xaml::Input;
using namespace Windows::UI::Xaml::Controls;
using namespace Windows::UI::ViewManagement;

using namespace Windows::Foundation;
using namespace Windows::Foundation::Numerics;

class PlaceHolderListener : public IPasteboardListener {};

static Thickness default_padding(4.0, 4.0, 4.0, 4.0);
static CanvasStrokeStyle^ dash_stroke = nullptr;
static ICanvasBrush^ border_color = nullptr;
static ICanvasBrush^ inset_border_color = nullptr;
static ICanvasBrush^ rubberband_color = nullptr;

#define REMOVE(ptr, refcount) if (ptr->refcount <= 1) { delete ptr; } else { ptr->refcount -= 1; }

/*************************************************************************************************/
struct SnipInfo : public AbstractObject {
    Pasteboard^ master;
    float x = 0.0F;
    float y = 0.0F;
    bool selected = false;
};

#define SNIP_INFO(snip) ((SnipInfo*)snip->info)

static inline void bind_snip_owership(Pasteboard^ master, Snip* snip) {
    SnipInfo* info = new SnipInfo();
    info->master = master;
    snip->info = info;
}

static inline void unsafe_move_snip_info(SnipInfo* info, float x, float y, bool absolute) {
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

static inline void unsafe_add_selected(IPasteboardListener* listener, Snip* snip, SnipInfo* info) {
    listener->before_select(info->master, snip);
    info->selected = true;
    info->master->refresh();
    listener->after_select(info->master, snip);
}

static inline void unsafe_set_selected(IPasteboardListener* listener, Snip* snip, SnipInfo* info) {
    info->master->begin_edit_sequence();
    info->master->no_selected();
    unsafe_add_selected(listener, snip, info);
    info->master->end_edit_sequence();
}

/*************************************************************************************************/
Pasteboard::Pasteboard(Panel^ parent, Platform::String^ id, IPasteboardLayout* layout) : Win2DCanvas(parent, id) {
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

        this->begin_edit_sequence();
        bind_snip_owership(this, snip);
        unsafe_move_snip_info(SNIP_INFO(snip), x, y, true);
        this->size_cache_invalid();
        this->refresh();
        this->end_edit_sequence();
        this->layout->after_insert(this, snip, x, y);
    }
}

void Pasteboard::move_to(Snip* snip, float x, float y) {
    if ((snip != nullptr) && (snip->info != nullptr)) {
        SnipInfo* info = SNIP_INFO(snip);
        if (info->master == this) {
            unsafe_move_snip_info(info, x, y, true);
        }
    }
}

void Pasteboard::move(Snip* snip, float x, float y) {
    if (snip != nullptr) {
        SnipInfo* info = SNIP_INFO(snip);
        if ((info != nullptr) && (info->master == this)) {
            unsafe_move_snip_info(info, x, y, false);
        }
    } else if (this->head_snip != nullptr) {
        Snip* child = this->head_snip;

        this->begin_edit_sequence();
        do {
            SnipInfo* info = SNIP_INFO(child);
            if (info->selected) {
                unsafe_move_snip_info(info, x, y, false);
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
        Snip* child = this->head_snip->prev;

        do {
            SnipInfo* info = SNIP_INFO(child);
            child->fill_extent(&width, &height);

            if ((info->x < x) && (x < (info->x + width)) && (info->y < y) && (y < (info->y + height))) {
                found = child;
                break;
            }

            child = child->prev;
        } while (child != this->head_snip->prev);
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
                SnipInfo* info = SNIP_INFO(child);
                child->fill_extent(&width, &height);
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
        SnipInfo* info = SNIP_INFO(snip);
        if ((info != nullptr) && (info->master == this) && (!info->selected)) {
            if (this->rubberband_allowed && this->listener->can_select(this, snip)) {
                unsafe_add_selected(this->listener, snip, info);
            }
        }
    }
}

void Pasteboard::set_selected(Snip* snip) {
    if (snip != nullptr) {
        SnipInfo* info = SNIP_INFO(snip);
        if ((info != nullptr) && (info->master == this) && (!info->selected)) {
            if (this->listener->can_select(this, snip)) {
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
            SnipInfo* info = SNIP_INFO(child);
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
void Pasteboard::set_pointer_listener(IPasteboardListener* listener) {
    if (this->listener != nullptr) {
        REMOVE(this->listener, refcount);
    }

    this->listener = listener;
    this->listener->refcount += 1;
    this->rubberband_allowed = this->listener->can_select_multiple(this);
}

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
                    SnipInfo* info = SNIP_INFO(snip);
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

/*************************************************************************************************/
void Pasteboard::draw(CanvasDrawingSession^ ds) {
    float Width = this->actual_layer_width;
    float Height = this->actual_layer_height;
    float tx = (float)this->inset.Left;
    float ty = (float)this->inset.Top;
    float width, height;

    // https://blogs.msdn.microsoft.com/win2d/2014/09/15/why-does-win2d-include-three-different-sets-of-vector-and-matrix-types/
    ds->Transform = make_float3x2_translation(tx, ty);
    
    { // draw borders
        if (dash_stroke == nullptr) {
            auto systemUI = ref new UISettings();

            dash_stroke = ref new CanvasStrokeStyle();
            dash_stroke->DashStyle = CanvasDashStyle::Dash;
            inset_border_color = ref new CanvasSolidColorBrush(ds, systemUI->GetColorValue(UIColorType::Accent));
            border_color = ref new CanvasSolidColorBrush(ds, systemUI->GetColorValue(UIColorType::AccentDark1));
        }

        ds->DrawRectangle(0.0F, 0.0F, Width, Height, inset_border_color, 1.0F, dash_stroke);
        ds->DrawRectangle(-tx, -ty, (float)this->actual_width, (float)this->actual_height, border_color);
    }

    auto region = ds->CreateLayer(1.0F, Rect(0.0F, 0.0F, Width, Height));

    { // draw minimum enclosing box
        float x, y;

        this->fill_snips_bounds(&x, &y, &width, &height);
        ds->FillRectangle(x, y, width, height, Colors::Snow);
        ds->DrawRectangle(x, y, width, height, Colors::MistyRose, 1.0F, dash_stroke);
    }

    if (this->head_snip != nullptr) {
        Snip* child = this->head_snip->prev;

        do {
            SnipInfo* info = SNIP_INFO(child);
            child->fill_extent(&width, &height);
            width = std::min(Width - info->x, width);
            height = std::min(Height - info->y, height);

            if ((info->x < Width) && (info->y < Height) && ((info->x + width) > 0) && ((info->y + height) > 0)) {
                auto layer = ds->CreateLayer(1.0F, Rect(info->x, info->y, width, height));
                child->draw(ds, info->x, info->y, width, height);
                if (info->selected) {
                    ds->FillCircle(info->x + width / 2.0F, info->y + height / 2.0F, 4.0F, Colors::White);
                }
                delete layer; /* Must Close the Layer Explicitly */
            }

            child = child->prev;
        } while (child != this->head_snip->prev);
    }

    if (this->rubberband_y != nullptr) {
        float left = std::min(this->last_pointer_x, (*this->rubberband_x));
        float top = std::min(this->last_pointer_y, (*this->rubberband_y));
        float width = std::abs((*this->rubberband_x) - this->last_pointer_x);
        float height = std::abs((*this->rubberband_y) - this->last_pointer_y);
        
        if (rubberband_color == nullptr) {
            auto systemUI = ref new UISettings();
            rubberband_color = ref new CanvasSolidColorBrush(ds, systemUI->UIElementColor(UIElementType::Highlight));
        }
        
        rubberband_color->Opacity = 0.32F;
        ds->FillRectangle(left, top, width, height, rubberband_color);
        rubberband_color->Opacity = 1.00F;
        ds->DrawRectangle(left, top, width, height, rubberband_color);
    }

    delete region; /* Must Close the Layer Explicitly */
}

