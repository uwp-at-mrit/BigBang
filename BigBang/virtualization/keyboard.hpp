#pragma once

#include "forward.hpp"
#include "snip/snip.hpp"

namespace WarGrey::SCADA {
	private enum class ScreenKeyboard { Numpad };

	private struct KeyboardCell {
		Windows::System::VirtualKey key;
		unsigned char col;
		unsigned char row;
		unsigned char ncol;
		unsigned char nrow;
	};

	private class IKeyboard abstract : public WarGrey::SCADA::ISprite {
	public:
		IKeyboard(WarGrey::SCADA::IPlanet* master);
		WarGrey::SCADA::Syslog* get_logger() override;

	public:
		void show(bool shown);
		bool shown();

	public:
		virtual void fill_auto_position(float* x, float* y) = 0;
		virtual bool is_colliding_with_mouse(float mouse_x, float mouse_y, float x, float y) = 0;
		
	protected:
		virtual void create() = 0;
		virtual Windows::System::VirtualKey find_tapped_key(float mouse_x, float mouse_y) = 0;

	protected:
		Windows::System::VirtualKey current_key;
		WarGrey::SCADA::IPlanet* master;

	private:
		bool _shown;
	};

	private class Keyboard abstract : public WarGrey::SCADA::IKeyboard {
	public:
		~Keyboard();
		Keyboard(IPlanet* master, KeyboardCell* keys, unsigned int keynum);

	public:
		void construct() override;
		void update(long long count, long long interval, long long uptime, bool is_slow) override;
		void draw(Microsoft::Graphics::Canvas::CanvasDrawingSession^ ds, float x, float y, float Width, float Height) override;
		bool is_colliding_with_mouse(float mouse_x, float mouse_y, float x, float y) override;

	public:
		void on_hover(float local_x, float local_y, bool shifted, bool controled) override;
		void on_tap(float local_x, float local_y, bool shifted, bool controled) override;
		void on_goodbye(float local_x, float local_y, bool shifted, bool controled) override;

	protected:
		virtual void draw_before(Microsoft::Graphics::Canvas::CanvasDrawingSession^ ds, float x, float y, float Width, float Height) {}
		virtual void draw_after(Microsoft::Graphics::Canvas::CanvasDrawingSession^ ds, float x, float y, float Width, float Height) {}

		virtual void draw_cell(Microsoft::Graphics::Canvas::CanvasDrawingSession^ ds,
			Windows::System::VirtualKey key, bool focused, bool tapped,
			float x, float y, float width, float height) = 0;

	protected:
		Windows::System::VirtualKey find_tapped_key(float mouse_x, float mouse_y);
		float cellsize;
		float gapsize;
		float em;

	private:
		WarGrey::SCADA::KeyboardCell* cells;
		unsigned int keynum;
		float* cell_boxes;

	private:
		bool tapped;
		long long uptime;
		long long taptime;
	};
}
