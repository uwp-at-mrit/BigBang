#include "pasteboard.h"

using namespace std;
using namespace Platform;

using namespace Win2D::UIElement;

using namespace Windows::UI;
using namespace Windows::UI::Xaml::Controls;

using namespace Microsoft::Graphics::Canvas;

struct SnipInfo {
    float x;
    float y;
};

IPasteboard::IPasteboard(Panel^ parent, String^ id) : Win2DCanvas(parent, id) {
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
    Snip* child = this->tailSnip;
    while (child != nullptr) {
        SnipInfo* info = (SnipInfo*)child->info;
        child->Draw(ds, info->x, info->y);
        child = child->prev;
    }

    ds->DrawRectangle(0.0f, 0.0f, (float)this->Control->ActualWidth, (float)this->Control->ActualHeight, Colors::SkyBlue);
}

/*************************************************************************************************/
Pasteboard::Pasteboard(Panel^ parent, String^ id) : IPasteboard(parent, id) {
}

VerticalPasteboard::VerticalPasteboard(Panel^ parent, String^ id) : VerticalPasteboard(parent, id, 0.0f) {}
VerticalPasteboard::VerticalPasteboard(Panel^ parent, String^ id, float gapsize) : IPasteboard(parent, id) {
    this->gapsize = gapsize;
    this->lastPosition = -gapsize;
}

void VerticalPasteboard::BeforeInsert(Snip* snip, float x, float y) {
    this->BeginEditSequence();
}

void VerticalPasteboard::AfterInsert(Snip* snip, float, float) {
    float x = 0.0f;
    float y = lastPosition + gapsize;
    float height = 0.0;
    
    snip->FillExtent(x, y, nullptr, &height);
    this->MoveTo(snip, x, y);
    this->lastPosition += (gapsize + height);
    this->EndEditSequence();
}
