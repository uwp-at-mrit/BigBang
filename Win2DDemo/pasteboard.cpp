#include "pasteboard.h"

using namespace std;
using namespace Platform;

using namespace Win2D::UIElement;

using namespace Windows::UI;
using namespace Windows::UI::Xaml::Controls;

using namespace Microsoft::Graphics::Canvas;

IPasteboard::IPasteboard(Panel^ parent, String^ id) : Win2DCanvas(parent, id) {
}

IPasteboard::~IPasteboard() {
    while (headSnip != nullptr) {
        Snip* snip = headSnip;
        headSnip = snip->next;

        delete snip;
    }
}

void IPasteboard::Insert(Snip* snip, float x, float y) {
    BeforeInsert(snip, x, y);
    
    if (tailSnip == nullptr) tailSnip = snip;
    snip->next = headSnip;
    if (headSnip != nullptr) headSnip->prev = snip;
    headSnip = snip;

    MoveTo(snip, x, y);

    AfterInsert(snip, x, y);
}

void IPasteboard::MoveTo(Snip* snip, float x, float y) {
    Refresh();
}

void IPasteboard::Draw(CanvasDrawingSession^ ds) {
    Snip* child = headSnip;
    while (child != nullptr) {
        child->Draw(ds, 0.0f, 0.0f);
        child = child->next;
    }

    ds->DrawRectangle(0.0f, 0.0f, (float)this->Control->ActualWidth, (float)this->Control->ActualHeight, Colors::SkyBlue);
}

/*************************************************************************************************/
Pasteboard::Pasteboard(Panel^ parent, String^ id) : IPasteboard(parent, id) {
}

VerticalPasteboard::VerticalPasteboard(Panel^ parent, String^ id, int gapsize) : IPasteboard(parent, id) {
    this->gapsize = gapsize;
    this->lastPosition = -gapsize;
}

void VerticalPasteboard::BeforeInsert(Snip* snip, float x, float y) {
    this->BeginEditSequence();
}

void VerticalPasteboard::AfterInsert(Snip* snip, float x, float y) {
    
    this->EndEditSequence();
}
