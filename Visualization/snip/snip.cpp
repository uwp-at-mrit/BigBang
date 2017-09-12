#include "snip.hpp"

using namespace WarGrey::WinACS;

using namespace Windows::UI;

SnipIcon::~SnipIcon() {}

SnipIcon::SnipIcon(float size, Color color) : size(size), color(color) {}

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
