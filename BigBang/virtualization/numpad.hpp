#pragma once

#include "virtualization/keyboard.hpp"

namespace WarGrey::SCADA {
    private class Numpad : public WarGrey::SCADA::IKeyboard {
    public:
		Numpad(IPlanet* master, float fontsize = 32.0F);

    public:
		void fill_extent(float x, float y, float* w = nullptr, float* h = nullptr) override;
		void update(long long count, long long interval, long long uptime, bool is_slow) override;
		void draw(Microsoft::Graphics::Canvas::CanvasDrawingSession^ ds, float x, float y, float Width, float Height) override;
		void fill_auto_position(float* x, float* y) override;
		
	public:
		void on_hover(float local_x, float local_y, bool shifted, bool controled) override;
		void on_tap(float local_x, float local_y, bool shifted, bool controled) override;
		void on_goodbye() override;

	protected:
		void create() override;
		void fill_cell(KeyboardCell* cell, unsigned int idx) override;

    private:
		Microsoft::Graphics::Canvas::Text::CanvasTextFormat^ label_font;
		Microsoft::Graphics::Canvas::Brushes::ICanvasBrush^ foreground;
		Microsoft::Graphics::Canvas::Brushes::ICanvasBrush^ background;
		Microsoft::Graphics::Canvas::Brushes::ICanvasBrush^ border;
		Microsoft::Graphics::Canvas::Brushes::ICanvasBrush^ highlight;
		Microsoft::Graphics::Canvas::Brushes::ICanvasBrush^ taplight;

	private:
		float cellsize;
		float gapsize;
		float radius;
		float em;

	private:
		bool tapped;
		long long uptime;
		long long taptime;
		int current_cell;
	};
}
