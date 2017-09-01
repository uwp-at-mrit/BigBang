#include "snip/snip.hpp"

using namespace std;
using namespace Platform;

using namespace Win2D::UIElement;

using namespace Microsoft::Graphics::Canvas;

SnipIcon::~SnipIcon() {}

SnipIcon::SnipIcon(float size) {
    this->size = size;
}

SnipTypes SnipIcon::get_type() {
    return SnipTypes::Icon;
}

void SnipIcon::fill_extent(float* width, float* height, float* descent, float* space, float* lspace, float* rspace) {
    if (width != nullptr) (*width) = this->size;
    if (height != nullptr) (*height) = this->size;
    if (descent != nullptr) (*descent) = 0.0f;
    if (space != nullptr) (*space) = 0.0f;
    if (lspace != nullptr) (*lspace) = 0.0f;
    if (rspace != nullptr) (*rspace) = 0.0f;
};