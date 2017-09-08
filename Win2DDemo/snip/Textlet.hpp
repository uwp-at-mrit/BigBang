#pragma once

#include "snip.hpp"

namespace WarGrey::Win2DDemo {
    WarGrey::Win2DDemo::SnipIcon* make_textlet_icon(float size, Windows::UI::Color color);

    private class Textlet : public WarGrey::Win2DDemo::Snip {
    public:
        ~Textlet() noexcept;
        Textlet(const wchar_t* fmt, ...);
        Textlet(Platform::String^ content = "");
        WarGrey::Win2DDemo::SnipTypes get_type() override;

    public:
        void change_text(Platform::String^ content);

    public:
        void fill_extent(float* w = nullptr, float* h = nullptr,
            float* d = nullptr, float* s = nullptr, float* l = nullptr, float* r = nullptr)
            override;

        void draw(Microsoft::Graphics::Canvas::CanvasDrawingSession^ ds, float x, float y, float Width, float Height) override;

    private:
        Platform::String^ content;
        Microsoft::Graphics::Canvas::Text::CanvasTextFormat^ layout_config;
    };

    private class TextIcon : public WarGrey::Win2DDemo::SnipIcon {
    public:
        TextIcon(float size, Windows::UI::Color color);
        ~TextIcon() noexcept;

    public:
        void draw(Microsoft::Graphics::Canvas::CanvasDrawingSession^ ds, float x, float y, float Width, float Height) override;

    public:
        Snip* create_snip() override;

    private:
        Windows::UI::Color foreground;
    };
}
