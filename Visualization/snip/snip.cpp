#include "snip.hpp"

using namespace WarGrey::SCADA;

Snip::~Snip() {
    if (info != nullptr) {
        delete info;
    }
}
