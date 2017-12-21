#pragma once

#include "snip.hpp"
#include "text.hpp"

namespace WarGrey::SCADA {
	private enum ArrowPosition { Start, End };

    private class DoubleArrowlet : public WarGrey::SCADA::Snip {
    public:
        DoubleArrowlet(float length, WarGrey::SCADA::ArrowPosition, double color, double saturation, double lightness);

    public:
        void draw(Microsoft::Graphics::Canvas::CanvasDrawingSession^ ds, float x, float y, float Width, float Height) override;
        void fill_extent(float x, float y, float* w = nullptr, float* h = nullptr) override;

    private:
		WarGrey::SCADA::ArrowPosition position;
		float length;
		float arrowhead_size;
		float scale_width;
		float scale_height;

	private:
        float in_temperature;
		float out_temperature;

	private:
        Microsoft::Graphics::Canvas::Text::CanvasTextFormat^ scale_font;
		Microsoft::Graphics::Canvas::Brushes::ICanvasBrush^ arrow_brush;
		Microsoft::Graphics::Canvas::Brushes::ICanvasBrush^ pipe_brush;
    };
}
