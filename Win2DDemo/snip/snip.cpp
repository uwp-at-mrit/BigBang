#include "snip/snip.h"

using namespace std;
using namespace Platform;

using namespace Win2D::UIElement;

using namespace Microsoft::Graphics::Canvas;

SnipIcon::~SnipIcon() {}

SnipIcon::SnipIcon(float size) {
    this->size = size;
}

SnipTypes SnipIcon::GetType() {
    return SnipTypes::Icon;
}

void SnipIcon::FillExtent(float x, float y, float* width, float* height, float* d, float* s, float* l, float* r) {
    if (width != nullptr) (*width) = this->size;
    if (height != nullptr) (*height) = this->size;
};
