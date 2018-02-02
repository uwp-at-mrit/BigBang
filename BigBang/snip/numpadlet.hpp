#pragma once

#include "snip.hpp"

namespace WarGrey::SCADA {
    #define NUMPAD_KEYNUM 14

    private class Numpadlet : public WarGrey::SCADA::ISnip {
    public:
		Numpadlet(float fontsize = 32.0F);

    public:
		void construct() override;
		void fill_extent(float x, float y, float* w = nullptr, float* h = nullptr) override;
		void update(long long count, long long interval, long long uptime, bool is_slow) override;
		void draw(Microsoft::Graphics::Canvas::CanvasDrawingSession^ ds, float x, float y, float Width, float Height) override;

	public:
		void on_hover(float local_x, float local_y, bool shifted, bool controled) override;
		void on_tap(float local_x, float local_y, bool shifted, bool controled) override;
		void on_goodbye() override;

	public:
		void show(WarGrey::SCADA::ISnip* target_snip, float xoff = 0.0F, float yoff = 0.0F);

	private:
		bool has_focus();
		void fill_cells();
		int find_cell(float mouse_x, float mouse_y);

    private:
		Microsoft::Graphics::Canvas::Text::CanvasTextFormat^ label_font;
		Microsoft::Graphics::Canvas::Brushes::ICanvasBrush^ foreground;
		Microsoft::Graphics::Canvas::Brushes::ICanvasBrush^ background;
		Microsoft::Graphics::Canvas::Brushes::ICanvasBrush^ border;
		Microsoft::Graphics::Canvas::Brushes::ICanvasBrush^ highlight;
		Microsoft::Graphics::Canvas::Brushes::ICanvasBrush^ taplight;

	private:
		float cells[NUMPAD_KEYNUM][6];
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
