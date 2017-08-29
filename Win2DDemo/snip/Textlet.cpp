#include "Textlet.h"

using namespace Win2D::Sniplet;
using namespace Win2D::UIElement;

using namespace std;
using namespace Platform;
using namespace Windows::UI;
using namespace Microsoft::Graphics::Canvas;
using namespace Microsoft::Graphics::Canvas::Text;

Textlet::~Textlet() {}

Textlet::Textlet(String^ content = "") {
    _content = content;
    _font = ref new CanvasTextFormat();
    _font->WordWrapping = CanvasWordWrapping::NoWrap;
}

SnipTypes Textlet::GetType() {
    return SnipTypes::Text;
}

TextExtent Textlet::GetExtent(Microsoft::Graphics::Canvas::CanvasDrawingSession^ ds, double x, double y) {
    return Win2DCanvas::GetTextExtent(ds, _content, _font);
};

void Textlet::Draw(CanvasDrawingSession^ ds, double x, double y) {
    ds->DrawText(_content, (float)x, (float)y, Colors::Black, _font);
}
