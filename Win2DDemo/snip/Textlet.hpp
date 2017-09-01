#pragma once

#include "snip/snip.hpp"

namespace Win2D::Snip {
    Win2D::UIElement::SnipIcon* make_textlet_icon(float size, unsigned char r, unsigned char g, unsigned char b);

    private class Textlet : public Win2D::UIElement::Snip {
    public:
        ~Textlet() noexcept;
        Textlet(const wchar_t* fmt, ...);
        Textlet(Platform::String^ content = "");
        Win2D::UIElement::SnipTypes get_type() override;

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

    private class TextIcon : public Win2D::UIElement::SnipIcon {
    public:
        TextIcon(float size, unsigned char r, unsigned char g, unsigned char b);

    public:
        void draw(Microsoft::Graphics::Canvas::CanvasDrawingSession^ ds, float x, float y, float Width, float Height) override;

    private:
        Windows::UI::Color foreground;
    };
}
