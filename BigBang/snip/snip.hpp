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
		virtual ~ISnip() noexcept;

	public:
		// `id` is designed for user-applications, in order to distinguish instances of a snip class.
		// User-Applications should define and maintain the enumerations on their own.
		long int id = -1L;

	public:
		void enable_events(bool yes) { this->deal_with_events = yes; }
		bool handles_events() { return this->deal_with_events; }

	public:
		Microsoft::Graphics::Canvas::CanvasRenderTarget^ take_snapshot(float dpi = 96.0F);
		void save(Platform::String^ path, float dpi = 96.0F);

    public:
        virtual void construct() {}
		virtual void fill_extent(float x, float y, float* width = nullptr, float* height = nullptr) = 0;
		virtual void update(long long count, long long interval, long long uptime, bool is_slow) {}
        virtual void draw(Microsoft::Graphics::Canvas::CanvasDrawingSession^ ds, float x, float y, float Width, float Height) = 0;
        
	public:
		virtual void own_caret(bool is_own) {}
		virtual void on_hover(float local_x, float local_y, bool shifted, bool controled) {};
		virtual void on_tap(float local_x, float local_y, bool shifted, bool controled) {};
		virtual void on_right_tap(float local_x, float local_y, bool shifted, bool controled) {};
		virtual void on_goodbye() {};

    public:
        ISnipInfo* info;

	private:
		bool deal_with_events = false;
    };

	private class IPipeSnip : public WarGrey::SCADA::ISnip {
	public:
		virtual Windows::Foundation::Rect get_input_port() = 0;
		virtual Windows::Foundation::Rect get_output_port() = 0;
	};
}
