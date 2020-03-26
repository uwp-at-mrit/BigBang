#pragma once

#include "forward.hpp"
#include "graphlet/primitive.hpp"

namespace WarGrey::SCADA {
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
		WarGrey::GYDM::Syslog* get_logger() override;

	public:
		void show(bool shown);
		bool shown();

	public:
		virtual void fill_auto_position(float* x, float* y, WarGrey::SCADA::IGraphlet* g, WarGrey::SCADA::GraphletAnchor a) = 0;
		virtual bool is_colliding_with_mouse(float mouse_x, float mouse_y, float keyboard_x, float keyboad_y) = 0;
		
	protected:
		virtual Windows::System::VirtualKey find_tapped_key(float mouse_x, float mouse_y) = 0;

	protected:
		Windows::System::VirtualKey current_key;
		WarGrey::SCADA::IPlanet* master;

	private:
		bool _shown;
	};

	private class Keyboard abstract : public WarGrey::SCADA::IKeyboard {
	public:
		virtual ~Keyboard();
		Keyboard(IPlanet* master, const KeyboardCell* keys, unsigned int keynum);

		template<size_t N>
		Keyboard(IPlanet* master, const KeyboardCell (&all_keys)[N]) : Keyboard(master, all_keys, N) {}

	public:
		void sprite() override;

	public:
		void construct() override;
		void update(long long count, long long interval, long long uptime) override;
		void fill_extent(float x, float y, float* w = nullptr, float* h = nullptr) override;
		void draw(Microsoft::Graphics::Canvas::CanvasDrawingSession^ ds, float x, float y, float Width, float Height) override;

	public:
		void fill_auto_position(float* x, float* y, WarGrey::SCADA::IGraphlet* g, WarGrey::SCADA::GraphletAnchor a) override;
		bool is_colliding_with_mouse(float mouse_x, float mouse_y, float x, float y) override;

	public:
		void on_hover(float local_x, float local_y) override;
		void on_tap(float local_x, float local_y) override;
		bool on_key(Windows::System::VirtualKey key, bool wargrey_keyboard) override;
		bool on_character(unsigned int keycode) override;
		void on_goodbye(float local_x, float local_y) override;

	protected:
		virtual void draw_before(Microsoft::Graphics::Canvas::CanvasDrawingSession^ ds, float x, float y, float Width, float Height);
		virtual void draw_after(Microsoft::Graphics::Canvas::CanvasDrawingSession^ ds, float x, float y, float Width, float Height) {}
		virtual void draw_cell(Microsoft::Graphics::Canvas::CanvasDrawingSession^ ds,
			Windows::System::VirtualKey key, bool focused, bool tapped,
			float x, float y, float width, float height);

	protected:
		virtual Microsoft::Graphics::Canvas::Text::CanvasTextLayout^ key_label(Windows::System::VirtualKey key) = 0;
		virtual Windows::System::VirtualKey find_received_key(unsigned int keycode);
		Windows::System::VirtualKey find_tapped_key(float mouse_x, float mouse_y);

	protected:
		Microsoft::Graphics::Canvas::Brushes::ICanvasBrush^ foreground;
		Microsoft::Graphics::Canvas::Brushes::ICanvasBrush^ background;
		Microsoft::Graphics::Canvas::Brushes::ICanvasBrush^ border;
		Microsoft::Graphics::Canvas::Brushes::ICanvasBrush^ highlight;
		Microsoft::Graphics::Canvas::Brushes::ICanvasBrush^ taplight;

	protected:
		float width;
		float height;
		float cellsize;
		float gapsize;
		float radius;

	private:
		const WarGrey::SCADA::KeyboardCell* cells;
		unsigned int keynum;
		Windows::Foundation::Rect* cell_boxes;

	private:
		bool tapped;
		long long uptime;
		long long taptime;
	};
}
