#pragma once

#include "snip.hpp"

namespace WarGrey::SCADA {
    WarGrey::SCADA::SnipIcon* make_alarmlet_icon(float size, Windows::UI::Color color);

    private class Alarmlet : public WarGrey::SCADA::Snip {
    public:
        ~Alarmlet() noexcept;
        Alarmlet(float size);
        WarGrey::SCADA::SnipTypes get_type() override;

    public:
        void change_text(Platform::String^ content);

    public:
        void fill_extent(float* w = nullptr, float* h = nullptr,
            float* d = nullptr, float* s = nullptr, float* l = nullptr, float* r = nullptr)
            override;

        void draw(Microsoft::Graphics::Canvas::CanvasDrawingSession^ ds, float x, float y, float Width, float Height) override;

    private:
        float size;
    };

    private class AlarmIcon : public WarGrey::SCADA::SnipIcon {
    public:
        AlarmIcon(float size, Windows::UI::Color color);
        ~AlarmIcon() noexcept;

    public:
        void draw(Microsoft::Graphics::Canvas::CanvasDrawingSession^ ds, float x, float y, float Width, float Height) override;

    public:
        Snip* create_snip() override;
    };
}
