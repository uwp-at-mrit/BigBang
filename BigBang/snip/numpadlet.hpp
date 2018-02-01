#pragma once

#include "snip.hpp"

namespace WarGrey::SCADA {
    private class Numpadlet : public WarGrey::SCADA::ISnip {
    public:
		Numpadlet(float cell_size = 48.0F);

    public:
		void construct() override;
		void fill_extent(float x, float y, float* w = nullptr, float* h = nullptr) override;
		void draw(Microsoft::Graphics::Canvas::CanvasDrawingSession^ ds, float x, float y, float Width, float Height) override;

	public:
		void show(WarGrey::SCADA::ISnip* target_snip, float xoff, float yoff);
        
    private:
		Microsoft::Graphics::Canvas::Text::CanvasTextFormat^ label_font;

	private:
		bool shown;
	};
}
