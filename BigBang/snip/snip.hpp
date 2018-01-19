#pragma once

#include "universe.hxx"
#include "forward.hpp"
#include "box.hpp"

namespace WarGrey::SCADA {
    private class ISnipInfo abstract {
    public:
		virtual ~ISnipInfo() noexcept {};
		ISnipInfo(IPlanet* master) : master(master) {};
		
    public:
		IPlanet* master;
    };

    private class ISnip abstract {
    public:
		virtual ~ISnip() noexcept {
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
    };

	private class IPipeSnip : public WarGrey::SCADA::ISnip {
	public:
		virtual Windows::Foundation::Rect get_input_port() = 0;
		virtual Windows::Foundation::Rect get_output_port() = 0;
	};
}
