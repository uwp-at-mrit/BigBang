#pragma once

#include "forward.hpp"

namespace WarGrey::SCADA {
    private class IUniverseLayout abstract {
    public:
        virtual void on_attach_to(Universe* master) {};
        virtual ~IUniverseLayout() noexcept {};

    public:
        virtual bool can_move(Universe* master, Windows::UI::Xaml::Input::PointerRoutedEventArgs^ e) { return true; };

    public:
        virtual void before_insert(Universe* master, Snip* snip, float x, float y) {};
        virtual void after_insert(Universe* master, Snip* snip, float x, float y) {};

    public:
        int refcount = 0;
    };
}
