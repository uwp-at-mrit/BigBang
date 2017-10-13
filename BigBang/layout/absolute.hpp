#pragma once

#include "layout.hpp"

namespace WarGrey::SCADA {
    private class AbsoluteLayout : public WarGrey::SCADA::IUniverseLayout {
    public:
        AbsoluteLayout(float min_width = 0.0F, float min_height = 0.0F);

    public:
        void before_insert(Universe* master, Snip* snip, float x, float y) override;
        void after_insert(Universe* master, Snip* snip, float x, float y) override;

    private:
        float preferred_min_width;
        float preferred_min_height;
    };
}
