#include "snip.hpp"

using namespace WarGrey::SCADA;

using namespace Microsoft::Graphics::Canvas;

Snip::~Snip() {
    if (info != nullptr) {
        delete info;
    }
}
