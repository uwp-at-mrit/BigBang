#pragma once

#include "decorator/decorator.hpp"

namespace WarGrey::SCADA {
    private class BorderDecorator sealed: public WarGrey::SCADA::IPasteboardDecorator {
    public:
        BorderDecorator(bool draw_outer = true, bool draw_inner = true, bool draw_enclosing = false);

    public:
        void draw_before(WarGrey::SCADA::Pasteboard^ master, Microsoft::Graphics::Canvas::CanvasDrawingSession^ ds,
            float width, float height, Windows::Foundation::Rect& outbox) override;

    private:
        bool draw_outer_border;
        bool draw_inner_border;
        bool draw_enclosing_box;
    };

    private class HBorderDecorator sealed : public WarGrey::SCADA::IPasteboardDecorator {
    public:
        HBorderDecorator(bool draw_top = true, bool draw_bottom = true, Windows::UI::Color& color = Windows::UI::Colors::Gray);

    public:
        void draw_before(WarGrey::SCADA::Pasteboard^ master, Microsoft::Graphics::Canvas::CanvasDrawingSession^ ds,
            float width, float height, Windows::Foundation::Rect& outbox) override;

    private:
        Windows::UI::Color border_color;
        bool draw_top_border;
        bool draw_bottom_border;
    };

    private class VBorderDecorator sealed : public WarGrey::SCADA::IPasteboardDecorator {
    public:
        VBorderDecorator(bool draw_left = true, bool draw_right = true, Windows::UI::Color& color = Windows::UI::Colors::Gray);

    public:
        void draw_before(WarGrey::SCADA::Pasteboard^ master, Microsoft::Graphics::Canvas::CanvasDrawingSession^ ds,
            float width, float height, Windows::Foundation::Rect& outbox) override;

    private:
        Windows::UI::Color border_color;
        bool draw_left_border;
        bool draw_right_border;
    };
}
