#include "Textlet.h"

using namespace Win2D::Sniplet;
using namespace Win2D::UIElement;

using namespace std;
using namespace Platform;
using namespace Windows::UI;
using namespace Microsoft::Graphics::Canvas;
using namespace Microsoft::Graphics::Canvas::Text;

Textlet::~Textlet() {}

Textlet::Textlet(String^ content) {
    _content = content;
    _font = ref new CanvasTextFormat();
    _font->WordWrapping = CanvasWordWrapping::NoWrap;
}

SnipTypes Textlet::GetType() {
    return SnipTypes::Text;
}

void Textlet::FillExtent(Microsoft::Graphics::Canvas::CanvasDrawingSession^ ds, float x, float y,
    float* width, float* height, float* descent, float* space, float* lspace, float* rspace) {
    TextExtent ts = Win2DCanvas::GetTextExtent(ds, _content, _font);

    if (width != nullptr) (*width) = ts.Width;
    if (height != nullptr) (*height) = ts.Height;
    if (descent != nullptr) (*descent) = ts.Descent;
    if (space != nullptr) (*space) = ts.Space;
    if (lspace != nullptr) (*lspace) = ts.LSpace;
    if (rspace != nullptr) (*rspace) = ts.RSpace;
};

void Textlet::Draw(CanvasDrawingSession^ ds, float x, float y) {
    ds->DrawText(_content, (float)x, (float)y, Colors::Black, _font);
}
