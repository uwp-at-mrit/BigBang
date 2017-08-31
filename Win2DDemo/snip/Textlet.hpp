#pragma once

#include "snip/snip.hpp"

namespace Win2D::Snip {
    private class Textlet : public Win2D::UIElement::Snip {
    public:
        static Win2D::UIElement::SnipIcon* CreateSnipIcon(float size, unsigned char r, unsigned char g, unsigned char b);

    public:
        ~Textlet() noexcept;
        Textlet(const wchar_t* fmt, ...);
        Textlet(Platform::String^ content = "");
        Win2D::UIElement::SnipTypes GetType() override;

    public:
        void SetText(Platform::String^ content);

    public:
        void FillExtent(float x, float y,
            float* w = nullptr, float* h = nullptr,
            float* d = nullptr, float* s = nullptr,
            float* l = nullptr, float* r = nullptr)
            override;

        void Draw(Microsoft::Graphics::Canvas::CanvasDrawingSession^ ds, float x, float y) override;

    private:
        Platform::String^ content;
        Microsoft::Graphics::Canvas::Text::CanvasTextFormat^ font;
    };

    private class TextIcon : public Win2D::UIElement::SnipIcon {
    public:
        TextIcon(float size, unsigned char r, unsigned char g, unsigned char b);

    public:
        void Draw(Microsoft::Graphics::Canvas::CanvasDrawingSession^ ds, float x, float y) override;

    private:
        Windows::UI::Color foreground;
    };
}
