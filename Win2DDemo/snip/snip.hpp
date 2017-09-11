#pragma once

#include "canvas.hxx"
#include "object.hpp"

namespace WarGrey::Win2DDemo {
    private enum SnipTypes { Text, Icon };

    private class Snip {
    public:
        virtual SnipTypes get_type() = 0;
        virtual ~Snip() noexcept { if (info != nullptr) delete info; };

    public:
        virtual void fill_extent(float* width = nullptr, float* height = nullptr,
            float* descent = nullptr, float* space = nullptr, float* lspace = nullptr, float* rspace = nullptr)
            = 0;

        virtual void draw(
            Microsoft::Graphics::Canvas::CanvasDrawingSession^ ds,
            float x, float y, float Width, float Height)
            = 0;

    public:
        AbstractObject* info;

    public:
        Snip* next;
        Snip* prev;
    };

    private class SnipIcon : public Snip {
    public:
        ~SnipIcon() noexcept;
        SnipIcon(float size, Windows::UI::Color color);
        Win2DDemo::SnipTypes get_type() override;

    public:
        void fill_extent(float* w = nullptr, float* h = nullptr,
            float* d = nullptr, float* s = nullptr, float* l = nullptr, float* r = nullptr)
            override;

    public:
        virtual Snip* create_snip() = 0;

    protected:
        float size;
        Windows::UI::Color color;
    };
}
