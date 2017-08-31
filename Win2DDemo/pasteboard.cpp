#include <algorithm>
#include <WindowsNumerics.h>

#include "pasteboard.hpp"

using namespace std;
using namespace Platform;

using namespace Win2D::UIElement;

using namespace Windows::UI;
using namespace Windows::UI::Xaml;
using namespace Windows::UI::Xaml::Controls;
using namespace Windows::Foundation;
using namespace Windows::Foundation::Numerics;

using namespace Microsoft::Graphics::Canvas;

struct SnipInfo {
    float x;
    float y;
};

static Thickness zero(0.0, 0.0, 0.0, 0.0);

IPasteboard::IPasteboard(Panel^ parent, String^ id) : IPasteboard(parent, id, zero) {}

IPasteboard::IPasteboard(Panel^ parent, String^ id, Thickness padding) : Win2DCanvas(parent, id) {
    this->Inset = padding;
}

IPasteboard::~IPasteboard() {
    while (this->headSnip != nullptr) {
        Snip* snip = this->headSnip;
        this->headSnip = snip->next;

        delete snip;
    }
}

void IPasteboard::Insert(Snip* snip, float x, float y) {
    this->BeforeInsert(snip, x, y);
    
    if (this->tailSnip == nullptr) this->tailSnip = snip;
    snip->next = this->headSnip;
    if (this->headSnip != nullptr) this->headSnip->prev = snip;
    this->headSnip = snip;

    snip->info = (void *)new SnipInfo();
    this->MoveTo(snip, x, y);

    this->AfterInsert(snip, x, y);
}

void IPasteboard::MoveTo(Snip* snip, float x, float y) {
    SnipInfo* info = (SnipInfo*)snip->info;

    info->x = x;
    info->y = y;
    this->Refresh();
}

void IPasteboard::Draw(CanvasDrawingSession^ ds) {
    float Width = this->activeWidth;
    float Height = this->activeHeight;
    float tx = (float)this->Inset.Left;
    float ty = (float)this->Inset.Top;
    float width, height;

    // https://blogs.msdn.microsoft.com/win2d/2014/09/15/why-does-win2d-include-three-different-sets-of-vector-and-matrix-types/
    ds->Transform = make_float3x2_translation(tx, ty);
    auto activeRegion = ds->CreateLayer(1.0f, Rect(0.0f, 0.0f, Width, Height));
    for (Snip* child = this->tailSnip; child != nullptr; child = child->prev) {
        SnipInfo* info = (SnipInfo*)child->info;
        child->FillExtent(info->x, info->y, &width, &height);
        width = max(Width - info->x, width);
        height = max(Height - info->y, height);
        auto layer = ds->CreateLayer(1.0f, Rect(info->x, info->y, width, height));
        child->Draw(ds, info->x, info->y);
        delete layer /* Must Close the Layer Explicitly */;
    }
    delete activeRegion;
    
    ds->DrawRectangle(0.0f, 0.0f, Width, Height, Colors::SkyBlue);
    ds->DrawRectangle(-tx, -ty, (float)Control->ActualWidth, (float)Control->ActualHeight, Colors::RoyalBlue);
}

/*************************************************************************************************/
Pasteboard::Pasteboard(Panel^ parent, String^ id) : Pasteboard(parent, id, zero) {}
Pasteboard::Pasteboard(Panel^ parent, String^ id, Thickness inset) : IPasteboard(parent, id, inset) {}

void Pasteboard::ChangeSize(double width, double height) {
    this->Control->Width = width;
    this->Control->Height = height;
}

VPasteboard::VPasteboard(Panel^ parent, String^ id) : VPasteboard(parent, id, 0.0f) {}
VPasteboard::VPasteboard(Panel^ parent, String^ id, float gapsize) : VPasteboard(parent, id, gapsize, zero) {}
VPasteboard::VPasteboard(Panel^ parent, String^ id, float gapsize, Thickness inset) : IPasteboard(parent, id, inset) {
    this->gapsize = gapsize;
    this->lastPosition = -gapsize;
}

void VPasteboard::BeforeInsert(Snip* snip, float x, float y) {
    this->BeginEditSequence();
}

void VPasteboard::AfterInsert(Snip* snip, float, float) {
    float Width = this->activeWidth;
    float x = 0.0f;
    float y = lastPosition + gapsize;
    float width = 0.0;
    float height = 0.0;
    
    snip->FillExtent(x, y, &width, &height);
    this->MoveTo(snip, x, y);
    this->lastPosition += (gapsize + height);
    this->minWidth = max(Width, width);
    this->minHeight = this->lastPosition;
    this->EndEditSequence();
}

void VPasteboard::ChangeSize(double width, double height) {
    this->Control->Height = height;
}
