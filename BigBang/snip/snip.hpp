#pragma once

#include "universe.hxx"
#include "forward.hpp"
#include "box.hpp"

namespace WarGrey::SCADA {
    private class ISnipInfo abstract {
    public:
        ISnipInfo(IPlanet* master) : master(master) {};
		virtual ~ISnipInfo() noexcept {};

    public:
		IPlanet* master;
    };

    private class Snip abstract {
    public:
		virtual ~Snip() noexcept {
			if (this->info != nullptr) {
				delete this->info;
				this->info = nullptr;
			}
		}

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

	private class IPipeSnip : public WarGrey::SCADA::Snip {
	public:
		virtual Windows::Foundation::Rect get_input_port() = 0;
		virtual Windows::Foundation::Rect get_output_port() = 0;
	};
}
