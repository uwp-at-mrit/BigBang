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
		virtual void resize(float width, float height) {}
		virtual void update(long long count, long long interval, long long uptime) {}
		virtual void draw(Microsoft::Graphics::Canvas::CanvasDrawingSession^ ds, float x, float y, float Width, float Height) = 0;
		virtual void draw_progress(Microsoft::Graphics::Canvas::CanvasDrawingSession^ ds, float x, float y, float Width, float Height) {}
		virtual bool ready() { return true; }

	public:
		virtual bool on_key(Windows::System::VirtualKey key, bool screen_keyboard) { return false; }
		virtual bool on_character(unsigned int keycode) { return false; }
		virtual bool on_wheel_translation(float local_x, float local_y, float delta, bool horizontal) { return false; }
		virtual bool on_wheel_zoom(float local_x, float local_y, float delta) { return false; }
		virtual void on_hover(float local_x, float local_y) {}
		virtual void on_tap(float local_x, float local_y) {}
		virtual void on_goodbye(float local_x, float local_y) {}

	public:
		virtual bool on_pointer_moved(float x, float y,
			Windows::Devices::Input::PointerDeviceType type,
			Windows::UI::Input::PointerUpdateKind puk)
		{ return false; }

		virtual bool on_pointer_pressed(float x, float y,
			Windows::Devices::Input::PointerDeviceType type,
			Windows::UI::Input::PointerUpdateKind puk)
		{ return false; }

		virtual bool on_pointer_released(float x, float y,
			Windows::Devices::Input::PointerDeviceType type,
			Windows::UI::Input::PointerUpdateKind puk)
		{ return false; }

		virtual bool on_pointer_moveout(float x, float y,
			Windows::Devices::Input::PointerDeviceType type,
			Windows::UI::Input::PointerUpdateKind puk)
		{ return false; }

		virtual bool on_pointer_wheeled(float x, float y, float delta,
			bool horizontal, bool controlled)
		{ return false; }

	public:
		void enable_resizing(bool yes_no, WarGrey::SCADA::GraphletAnchor anchor = GraphletAnchor::CC) { this->can_resize = yes_no; this->resize_anchor = anchor; }
		bool resizable(WarGrey::SCADA::GraphletAnchor* anchor) { (*anchor) = this->resize_anchor; return this->can_resize; }

	public:
		void enable_events(bool yes_no, bool low_level = false) { this->deal_with_events = yes_no; this->deal_with_low_level_events = low_level; }
		void disable_wheel_translation(bool yes_no) { this->wheel_translation = (!yes_no); }
		bool handle_events() { return this->deal_with_events; }
		bool handle_low_level_events() { return (this->handle_events() && this->deal_with_low_level_events); }
		bool handle_wheel_translation() { return this->wheel_translation; }

	public:
		void camouflage(bool yes_no) { this->findable = !yes_no; }
		bool concealled() { return !this->findable; }

	public:
		Microsoft::Graphics::Canvas::CanvasRenderTarget^ take_snapshot(float dpi = 96.0F);
		void save(Platform::String^ path, float dpi = 96.0F);

	private:
		WarGrey::SCADA::GraphletAnchor resize_anchor;
		bool can_resize = false;
		bool deal_with_events = false;
		bool deal_with_low_level_events = false;
		bool wheel_translation = true;
		bool findable = true;
	};
}
