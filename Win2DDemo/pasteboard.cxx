#include <algorithm>
#include <WindowsNumerics.h>

#include "pasteboard.hxx"
#include "layout/absolute.hxx"

using namespace std;
using namespace Platform;
using namespace WarGrey::Win2DDemo;

using namespace Windows::UI;
using namespace Windows::UI::Xaml;
using namespace Windows::UI::Xaml::Controls;
using namespace Windows::Foundation;
using namespace Windows::Foundation::Numerics;
using namespace Windows::System;
using namespace Windows::UI::Input;
using namespace Windows::Devices::Input;
using namespace Microsoft::Graphics::Canvas;

struct SnipInfo {
    float x;
    float y;
};

static Thickness default_padding(4.0, 4.0, 4.0, 4.0);
static IPasteboardLayout^ default_layout = ref new AbsoluteLayout();

Pasteboard::Pasteboard(Panel^ parent, String^ id) : Pasteboard(parent, id, default_layout) {}

Pasteboard::Pasteboard(Panel^ parent, String^ id, IPasteboardLayout^ layout) : Win2DCanvas(parent, id) {
    this->padding = default_padding;
    this->layout = layout;
}

Pasteboard::~Pasteboard() {
    while (this->head_snip != nullptr) {
        Snip* snip = this->head_snip;
        this->head_snip = snip->next;

        delete snip;
    }
}

void Pasteboard::insert(Snip* snip, float x, float y) {
    this->layout->before_insert(this, snip, x, y);
    
    if (this->tail_snip == nullptr) this->tail_snip = snip;
    snip->next = this->head_snip;
    if (this->head_snip != nullptr) this->head_snip->prev = snip;
    this->head_snip = snip;

    this->move(snip, x, y);
    this->layout->after_insert(this, snip, x, y);
}

void Pasteboard::move(Snip* snip, float x, float y) {
    SnipInfo* info = (SnipInfo*)snip->info;
    bool is_invalid = (info == nullptr);

    if (is_invalid) {
        info = new SnipInfo();
        snip->info = (void *)info;
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

void Pasteboard::draw(CanvasDrawingSession^ ds) {
    float Width = this->layer_width;
    float Height = this->layer_height;
    float tx = (float)this->inset.Left;
    float ty = (float)this->inset.Top;
    float width, height;

    // https://blogs.msdn.microsoft.com/win2d/2014/09/15/why-does-win2d-include-three-different-sets-of-vector-and-matrix-types/
    ds->Transform = make_float3x2_translation(tx, ty);
    auto activeRegion = ds->CreateLayer(1.0F, Rect(0.0F, 0.0F, Width, Height));
    for (Snip* child = this->tail_snip; child != nullptr; child = child->prev) {
        SnipInfo* info = (SnipInfo*)child->info;
        child->fill_extent(&width, &height);
        width = max(Width - info->x, width);
        height = max(Height - info->y, height);
        
        if ((info->x < Width) && (info->y < Height) && ((info->x + width) > 0) && ((info->y + height) > 0)) {
            auto layer = ds->CreateLayer(1.0F, Rect(info->x, info->y, width, height));
            child->draw(ds, info->x, info->y, Width, Height);
            delete layer /* Must Close the Layer Explicitly */;
        }
    }
    delete activeRegion;
    
    ds->DrawRectangle(0.0F, 0.0F, Width, Height, Colors::SkyBlue);
    ds->DrawRectangle(-tx, -ty, (float)canvas->ActualWidth, (float)canvas->ActualHeight, Colors::RoyalBlue);
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

void Pasteboard::fill_snips_extent(float* x, float* y, float* width, float* height) {
    this->recalculate_snips_extent_when_invalid();
    if (x != nullptr) (*x) = this->snips_x;
    if (y != nullptr) (*y) = this->snips_y;
    if (width != nullptr) (*width) = this->snips_width;
    if (height != nullptr) (*height) = this->snips_height;
}

void Pasteboard::size_cache_invalid() {
    this->snips_width = -1.0F;
}

void Pasteboard::recalculate_snips_extent_when_invalid() {
    if (this->snips_width < 0.0F) {
        float width, height;

        this->snips_x = FLT_MAX;
        this->snips_y = FLT_MAX;
        this->snips_width = 0.0F;
        this->snips_height = 0.0F;

        for (Snip* child = this->head_snip; child != nullptr; child = child->next) {
            SnipInfo* info = (SnipInfo*)child->info;
            child->fill_extent(&width, &height);
            this->snips_x = min(this->snips_x, info->x);
            this->snips_y = min(this->snips_y, info->y);
            this->snips_width = max(this->snips_width, info->x + width);
            this->snips_height = max(this->snips_height, info->y + height);
        }

        this->min_layer_width = this->snips_width;
        this->min_layer_height = this->snips_height;
    }
}

void Pasteboard::on_end_edit_sequence() {
    this->recalculate_snips_extent_when_invalid();
}

/*************************************************************************************************/
void Pasteboard::inset::set(Thickness v) { this->padding = v; }
Thickness Pasteboard::inset::get() { return this->padding; }

void Pasteboard::min_layer_width::set(float v) { this->canvas->MinWidth = double(v) + this->inset.Left + this->inset.Right; }
float Pasteboard::min_layer_width::get() { return float(this->canvas->MinWidth - this->inset.Left - this->inset.Right); }

void Pasteboard::min_layer_height::set(float v) { this->canvas->MinHeight = double(v) + this->inset.Top + this->inset.Bottom; }
float Pasteboard::min_layer_height::get() { return float(this->canvas->MinHeight - this->inset.Top - this->inset.Bottom); }

void Pasteboard::layer_width::set(float v) { this->canvas->Height = double(v) + this->inset.Top + this->inset.Bottom; }
float Pasteboard::layer_width::get() { return float(this->canvas->ActualWidth - this->inset.Left - this->inset.Right); }

void Pasteboard::layer_height::set(float v) { this->canvas->Height = double(v) + this->inset.Top + this->inset.Bottom; }
float Pasteboard::layer_height::get() { return float(this->canvas->ActualHeight - this->inset.Top - this->inset.Bottom); }
