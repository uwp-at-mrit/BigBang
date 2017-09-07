#include <algorithm>
#include <WindowsNumerics.h>

#include "pasteboard.hxx"
#include "layout/absolute.hpp"

using namespace std;
using namespace Platform;
using namespace WarGrey::Win2DDemo;

using namespace Windows::System;
using namespace Windows::Devices::Input;
using namespace Microsoft::Graphics::Canvas;
using namespace Microsoft::Graphics::Canvas::Geometry;

using namespace Windows::UI;
using namespace Windows::UI::Input;
using namespace Windows::UI::Xaml;
using namespace Windows::UI::Xaml::Input;
using namespace Windows::UI::Xaml::Controls;

using namespace Windows::Foundation;
using namespace Windows::Foundation::Numerics;

struct SnipInfo {
    float x = 0.0F;
    float y = 0.0F;
    bool selected = false;
    float move_x = 0.0F;
    float move_y = 0.0F;
};

static Thickness default_padding(4.0, 4.0, 4.0, 4.0);

Pasteboard::Pasteboard(Panel^ parent, String^ id, IPasteboardLayout* layout) : Win2DCanvas(parent, id) {
    this->padding = default_padding;
    this->layout = ((layout == nullptr) ? new AbsoluteLayout() : layout);
    this->layout->refcount += 1;
    this->layout->on_attach_to(this);

    this->control->PointerMoved += ref new PointerEventHandler(this, &Pasteboard::on_pointer_moved);
    this->control->PointerPressed += ref new PointerEventHandler(this, &Pasteboard::on_pointer_pressed);
    this->control->PointerReleased += ref new PointerEventHandler(this, &Pasteboard::on_pointer_released);
}

Pasteboard::~Pasteboard() {
    if (this->first_snip != nullptr) {
        Snip* child = nullptr;
        this->first_snip->prev->next = nullptr;
        do {
            child = this->first_snip;
            this->first_snip = this->first_snip->next;
            delete child;
        } while (this->first_snip != nullptr);
    }

    this->layout->refcount -= 1;
    if (this->layout->refcount == 0) {
        delete this->layout;
    }
}

void Pasteboard::insert(Snip* snip, float x, float y) {
    if (snip->info == nullptr) { // TODO: should it be copied if one snip can only belongs to one pasteboard
        this->layout->before_insert(this, snip, x, y);

        if (this->first_snip == nullptr) {
            this->first_snip = snip;
            snip->prev = this->first_snip;
        } else {
            snip->prev = this->first_snip->prev;
            this->first_snip->prev->next = snip;
            this->first_snip->prev = snip;
        }
        snip->next = this->first_snip;

        this->move(snip, x, y);
        this->layout->after_insert(this, snip, x, y);
    }
}

void Pasteboard::move(Snip* snip, float x, float y, bool relative) {
    SnipInfo* info = (SnipInfo*)snip->info;
    bool is_invalid = (info == nullptr);

    if (is_invalid) {
        info = new SnipInfo();
        snip->info = (void *)info;
    }

    if (relative) {
        x += info->x;
        y += info->y;
    }

    if ((info->x != x) || (info->y != y)) {
        info->x = x;
        info->y = y;
        is_invalid = true;
    }

    if (is_invalid) {
        this->size_cache_invalid();
        this->refresh();
    }
}

Snip* Pasteboard::find_snip(float x, float y) {
    float width, height;
    Snip* found = nullptr;

    if (this->first_snip != nullptr) {
        Snip* child = this->first_snip->prev;

        do {
            SnipInfo* info = (SnipInfo*)child->info;
            child->fill_extent(&width, &height);

            if ((info->x < x) && (x < (info->x + width)) && (info->y < y) && (y < (info->y + height))) {
                found = child;
                break;
            }

            child = child->prev;
        } while (child != this->first_snip->prev);
    }
    
    return found;
}

void Pasteboard::draw(CanvasDrawingSession^ ds) {
    float Width = this->actual_layer_width;
    float Height = this->actual_layer_height;
    float tx = (float)this->inset.Left;
    float ty = (float)this->inset.Top;
    float width, height;

    // https://blogs.msdn.microsoft.com/win2d/2014/09/15/why-does-win2d-include-three-different-sets-of-vector-and-matrix-types/
    ds->Transform = make_float3x2_translation(tx, ty);

    { // draw border and bounds
        float x, y;
        auto stroke = ref new CanvasStrokeStyle();
        stroke->DashStyle = CanvasDashStyle::Dash;

        this->fill_snips_bounds(&x, &y, &width, &height);
        ds->FillRectangle(x, y, width, height, Colors::Snow);
        ds->DrawRectangle(x, y, width, height, Colors::MistyRose, 1.0F, stroke);

        ds->DrawRectangle(0.0F, 0.0F, Width, Height, Colors::DeepSkyBlue);
        ds->DrawRectangle(-tx, -ty, (float)this->actual_width, (float)this->actual_height, Colors::RoyalBlue);
    }

    if (this->first_snip != nullptr) {
        auto region = ds->CreateLayer(1.0F, Rect(0.0F, 0.0F, Width, Height));
        Snip* child = this->first_snip->prev;

        do {
            SnipInfo* info = (SnipInfo*)child->info;
            child->fill_extent(&width, &height);
            width = max(Width - info->x, width);
            height = max(Height - info->y, height);

            if ((info->x < Width) && (info->y < Height) && ((info->x + width) > 0) && ((info->y + height) > 0)) {
                auto layer = ds->CreateLayer(1.0F, Rect(info->x, info->y, width, height));
                child->draw(ds, info->x, info->y, Width, Height);
                delete layer; /* Must Close the Layer Explicitly */
            }

            child = child->prev;
        } while (child != this->first_snip->prev);
        delete region; /* Must Close the Layer Explicitly */
    }
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
    this->preferred_min_width = max(min_width, 0.0F);
    this->preferred_min_height = max(min_height, 0.0F);
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

        if (this->first_snip == nullptr) {
            this->snips_left = 0.0F;
            this->snips_top = 0.0F;
            this->snips_right = 0.0F;
            this->snips_bottom = 0.0F;
        } else {
            Snip* child = this->first_snip;

            this->snips_left = FLT_MAX;
            this->snips_top = FLT_MAX;
            this->snips_right = -FLT_MAX;
            this->snips_bottom = -FLT_MAX;
            
            do {
                SnipInfo* info = (SnipInfo*)child->info;
                child->fill_extent(&width, &height);
                this->snips_left = min(this->snips_left, info->x);
                this->snips_top = min(this->snips_top, info->y);
                this->snips_right = max(this->snips_right, info->x + width);
                this->snips_bottom = max(this->snips_bottom, info->y + height);

                child = child->next;
            } while (child != this->first_snip);
        }

        this->min_layer_width = max(this->snips_right, this->preferred_min_width);
        this->min_layer_height = max(this->snips_bottom, this->preferred_min_height);
    }
}

void Pasteboard::on_end_edit_sequence() {
    this->recalculate_snips_extent_when_invalid();
}

void Pasteboard::set_selected(Snip* snip) {
    if (snip != nullptr) {
        this->cleanup_selection(snip, true);
    }
}

void Pasteboard::no_selected() {
    this->cleanup_selection(nullptr, false);
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

/************************************************************************************************/
void Pasteboard::set_pointer_listener(IPointerListener^ listener) {
    this->listener = listener;
}

void Pasteboard::on_pointer_moved(Object^ sender, PointerRoutedEventArgs^ e) {
    if (!e->Handled) {
        auto ppt = e->GetCurrentPoint(this->control);
        float x = ppt->Position.X;
        float y = ppt->Position.Y;

        if (ppt->Properties->IsLeftButtonPressed) {
            this->canvas_position_to_drawing_position(&x, &y);
            
            if (this->captured_snip != nullptr) {
                SnipInfo* info = (SnipInfo*)this->captured_snip->info;
                this->move(this->captured_snip, x - info->move_x, y - info->move_y, true);
                info->move_x = x;
                info->move_y = y;
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
        
        if (ppt->Properties->IsLeftButtonPressed && this->canvas_position_to_drawing_position(&x, &y)) {
            this->captured_snip = this->find_snip(x, y);

            if (this->captured_snip != nullptr) {
                SnipInfo* info = (SnipInfo*)this->captured_snip->info;
                info->move_x = x;
                info->move_y = y;
            }
        }

        e->Handled = true;
    }
}

void Pasteboard::on_pointer_released(Object^ sender, PointerRoutedEventArgs^ e) {
    if (!e->Handled) {
        if (this->captured_snip != nullptr) {
            this->captured_snip = nullptr;
        } else {
            auto ppt = e->GetCurrentPoint(this->control);
            float x = ppt->Position.X;
            float y = ppt->Position.Y;

            if (this->canvas_position_to_drawing_position(&x, &y)) {
                Snip* snip = this->find_snip(x, y);

                //if (snip != nullptr) {
                //  this->set_selected(snip);
                //} else {
                //  this->no_selected();
                //}
            }
        }

        e->Handled = true;
    }
}

void Pasteboard::cleanup_selection(Snip* snip, bool selection) {
    if (this->first_snip != nullptr) {
        Snip* child = this->first_snip;

        this->begin_edit_sequence();
        do {
            if ((snip == nullptr) || (snip == child)) {
                SnipInfo* info = (SnipInfo*)child->info;
                if (!info->selected ^ !selection) { /* xor */
                    info->selected = selection;
                    this->refresh();
                }

                if (snip != nullptr) break;
            }

            child = child->next;
        } while (child != this->first_snip);
        this->end_edit_sequence();
    }
}
