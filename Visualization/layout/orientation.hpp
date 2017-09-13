#pragma once

#include "layout.hpp"

namespace WarGrey::WinACS {
    private class VerticalLayout : public WarGrey::WinACS::IPasteboardLayout {
    public:
        VerticalLayout(float gap_size);

    public:
        bool can_move(Pasteboard^ self, Windows::UI::Xaml::Input::PointerRoutedEventArgs^ e) override;

    public:
        void before_insert(Pasteboard^ self, Snip* snip, float x, float y) override;
        void after_insert(Pasteboard^ self, Snip* snip, float x, float y) override;

    private:
        float gapsize;
    };

    private class HorizontalLayout : public WarGrey::WinACS::IPasteboardLayout {
    public:
        HorizontalLayout(float gap_size);

    public:
        bool can_move(Pasteboard^ self, Windows::UI::Xaml::Input::PointerRoutedEventArgs^ e) override;

    public:
        void before_insert(Pasteboard^ self, Snip* snip, float x, float y) override;
        void after_insert(Pasteboard^ self, Snip* snip, float x, float y) override;

    private:
        float gapsize;
    };
}
