#pragma once

#include "forward.hpp"
#include "syslog.hpp"

namespace WarGrey::SCADA {
	private class ISprite abstract {
	public:
		virtual void sprite() {}           // pseudo constructor for special derived classes before constructing
		virtual void sprite_construct() {} // pseudo constructor for special derived classes after constructing

	public:
		virtual WarGrey::SCADA::Syslog* get_logger() = 0;

	public:
		virtual void construct() {}
		virtual void fill_extent(float x, float y, float* width = nullptr, float* height = nullptr) = 0;
		virtual void fill_margin(float x, float y, float* top = nullptr, float* right = nullptr, float* bottom = nullptr, float* left = nullptr);
		virtual bool resize(float width, float height) { return false; }
		virtual void update(long long count, long long interval, long long uptime) {}
		virtual void draw(Microsoft::Graphics::Canvas::CanvasDrawingSession^ ds, float x, float y, float Width, float Height) = 0;
		virtual void draw_progress(Microsoft::Graphics::Canvas::CanvasDrawingSession^ ds, float x, float y, float Width, float Height) {}
		virtual bool ready() { return true; }

	public:
		virtual bool on_key(Windows::System::VirtualKey key, bool screen_keyboard) { return false; }
		virtual bool on_character(unsigned int keycode) { return false; }
		virtual void on_hover(float local_x, float local_y) {}
		virtual void on_tap(float local_x, float local_y) {}
		virtual void on_goodbye(float local_x, float local_y) {}

	public:
		void enable_events(bool yes) { this->deal_with_events = yes; }
		bool handles_events() { return this->deal_with_events; }

	public:
		Microsoft::Graphics::Canvas::CanvasRenderTarget^ take_snapshot(float dpi = 96.0F);
		void save(Platform::String^ path, float dpi = 96.0F);

	private:
		bool deal_with_events = false;
	};
}
