#include "pasteboard.h"
#include "layout.h"

using namespace std;
using namespace Platform;

using namespace Win2D::UIElement;

using namespace Windows::UI;
using namespace Windows::UI::Xaml::Controls;

using namespace Microsoft::Graphics::Canvas;

Pasteboard::Pasteboard(Panel^ parent, String^ id, Layout* layout) : Win2DCanvas(parent, id) {
    this->layout = (layout == nullptr) ? new Layout() : layout;
}

Pasteboard::~Pasteboard() {
    while (headSnip != nullptr) {
        Snip* snip = headSnip;
        headSnip = snip->next;

        delete snip;
    }

    delete layout;
}

void Pasteboard::Insert(Snip* snip, double x, double y) {
    this->BeforeInsert(snip, x, y);
    
    snip->next = headSnip;
    if (headSnip != nullptr) headSnip->prev = snip;
    headSnip = snip;
    MoveTo(snip, x, y);
    Refresh();

    this->AfterInsert(snip, x, y);
}

void Pasteboard::MoveTo(Snip* snip, double x, double y) {
    
}

void Pasteboard::Draw(CanvasDrawingSession^ ds) {
    Snip* child = headSnip;
    while (child != nullptr) {
        child->Draw(ds, 0.0, 0.0);
        child = child->next;
    }

    ds->DrawRectangle(0.0f, 0.0f, this->Control->ActualWidth, this->Control->ActualHeight, Colors::SkyBlue);
}
