#pragma once

#include "layout.hpp"

namespace WarGrey::SCADA {
    private class VerticalLayout : public WarGrey::SCADA::IUniverseLayout {
    public:
        VerticalLayout(float gap_size);

    public:
        bool can_move(Universe* self, Windows::UI::Xaml::Input::PointerRoutedEventArgs^ e) override;

    public:
        void after_insert(Universe* self, Snip* snip, float x, float y) override;

    private:
        float gapsize;
    };

    private class HorizontalLayout : public WarGrey::SCADA::IUniverseLayout {
    public:
        HorizontalLayout(float gap_size);

    public:
        bool can_move(Universe* self, Windows::UI::Xaml::Input::PointerRoutedEventArgs^ e) override;

    public:
        void after_insert(Universe* self, Snip* snip, float x, float y) override;

    private:
        float gapsize;
    };
}
