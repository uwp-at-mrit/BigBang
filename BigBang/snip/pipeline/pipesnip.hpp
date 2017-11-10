#pragma once

#include "snip/snip.hpp"
#include "snip/misc.hpp"
#include "rsyslog.hpp"

namespace WarGrey::SCADA {
    private class IPipeSnip : public WarGrey::SCADA::Snip {
    public:
        virtual Windows::Foundation::Rect get_inlet() = 0;
        virtual Windows::Foundation::Rect get_outlet() = 0;
    };

    template <class PS>
    private class HFlippedPipeSnip : public WarGrey::SCADA::IPipeSnip {
    public:
        HFlippedPipeSnip(PS* self) : master(self) {}
        ~HFlippedPipeSnip() noexcept { delete this->master; }
        PS* self() { return this->master; }

    public:
        void load() override {
            this->master->load();
        }

        void update(long long count, long long interval, long long uptime, bool is_slow) override {
            this->master->update(count, interval, uptime, is_slow);
        }

        void fill_extent(float x, float y, float* w = nullptr, float* h = nullptr) override {
            this->master->fill_extent(x, y, w, h);
        }

        void draw(Microsoft::Graphics::Canvas::CanvasDrawingSession^ ds, float x, float y, float Width, float Height) override {
            snip_draw_with_hflipping(this->master, ds, x, y, Width, Height);
        }

    public:
        Windows::Foundation::Rect get_inlet() override {
            return snip_socket_hflip(this->master, 0.0F, 0.0F, this->master->get_inlet());
        }

        Windows::Foundation::Rect get_outlet() override {
            return snip_socket_hflip(this->master, 0.0F, 0.0F, this->master->get_outlet());
        }
    
    private:
        PS* master;
    };

    void pipe_connecting_position(
        WarGrey::SCADA::IPipeSnip* prev,
        WarGrey::SCADA::IPipeSnip* pipe,
        float* x,
        float* y,
        double factor_x = 0.5,
        double factor_y = 0.5);

    Windows::Foundation::Numerics::float2 pipe_connecting_position(
        WarGrey::SCADA::IPipeSnip* prev,
        WarGrey::SCADA::IPipeSnip* pipe,
        float x = 0.0F,
        float y = 0.0F,
        double factor_x = 0.5,
        double factor_y = 0.5);
}
