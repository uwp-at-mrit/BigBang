#include <cwchar>
#include <cstdarg>
#include <cstdlib>

#include "Iconlet.h"
#include "canvas.h"

using namespace Win2D::Snip;
using namespace Win2D::UIElement;

using namespace std;
using namespace Platform;
using namespace Windows::UI;
using namespace Microsoft::Graphics::Canvas;
using namespace Microsoft::Graphics::Canvas::Text;

Iconlet::~Iconlet() {}

Iconlet::Iconlet(float size) {
    this->size = size;
}

SnipTypes Iconlet::GetType() {
    return SnipTypes::Icon;
}

void Iconlet::FillExtent(float x, float y, float* width, float* height, float* descent, float* space, float* lspace, float* rspace) {
    if (width != nullptr) (*width) = this->size;
    if (height != nullptr) (*height) = this->size;
};

void Iconlet::Draw(CanvasDrawingSession^ ds, float x, float y) {
    //ds->DrawText(content, (float)x, (float)y, Colors::Black, font);
}
