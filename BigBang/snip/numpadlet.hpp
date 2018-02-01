#pragma once

#include "snip.hpp"

namespace WarGrey::SCADA {
    private class Numpadlet : public WarGrey::SCADA::ISnip {
    public:
		Numpadlet(float fontsize = 32.0F);

    public:
		void construct() override;
		void fill_extent(float x, float y, float* w = nullptr, float* h = nullptr) override;
		void draw(Microsoft::Graphics::Canvas::CanvasDrawingSession^ ds, float x, float y, float Width, float Height) override;

	public:
		bool handles_events() override;
		void on_hover(float local_x, float local_y, bool shifted, bool controled) override;
		void on_tap(float local_x, float local_y, bool shifted, bool controled) override;
		void on_right_tap(float local_x, float local_y, bool shifted, bool controled) override;
		void on_goodbye() override;

	public:
		void show(WarGrey::SCADA::ISnip* target_snip, float xoff = 0.0F, float yoff = 0.0F);

	private:
		bool has_focus();
		void draw_cell(Microsoft::Graphics::Canvas::CanvasDrawingSession^ ds, float x, float y,
			Platform::String^ label, int col, int row, int ncol, int nrow);

    private:
		Microsoft::Graphics::Canvas::Text::CanvasTextFormat^ label_font;
		float cellsize;
		float gapsize;
		float radius;
		float em;
	};
}
