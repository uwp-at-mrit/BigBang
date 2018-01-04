#pragma once

#include "control.hxx"
#include "forward.hpp"
#include "box.hpp"

namespace WarGrey::SCADA {
    private class ISnipInfo abstract {
    public:
        ISnipInfo(IUniverse* master) : master(master) {};
		virtual ~ISnipInfo() noexcept {};

    public:
		IUniverse* master;
    };

    private class Snip abstract {
    public:
        virtual ~Snip() noexcept { if (this->info != nullptr) { delete this->info; } };

    public:
        virtual void load() {};
        virtual void update(long long count, long long interval, long long uptime, bool is_slow) {};
        virtual void draw(Microsoft::Graphics::Canvas::CanvasDrawingSession^ ds, float x, float y, float Width, float Height) = 0;
        virtual void fill_extent(float x, float y, float* width = nullptr, float* height = nullptr) = 0;
        
    public:
        ISnipInfo* info;

    public:
        Snip* next;
        Snip* prev;
    };
}
