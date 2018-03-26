#pragma once

#include "graphlet/primitive.hpp"

namespace WarGrey::SCADA {
    private class Gaugelet : public WarGrey::SCADA::IScalelet<float> {
    public:
        Gaugelet(Platform::String^ caption, int range, unsigned char step = 0,
			Windows::UI::Color& color = Windows::UI::Colors::Green);

	public:
        void construct() override;
		void fill_extent(float x, float y, float* w = nullptr, float* h = nullptr) override;
		void draw(Microsoft::Graphics::Canvas::CanvasDrawingSession^ ds, float x, float y, float Width, float Height) override;

	public:
		void update_scale() override;

    private:
        void initialize_meter();
        void draw_meter(Microsoft::Graphics::Canvas::CanvasDrawingSession^ ds,
            float x, float y, float scale, int range,
            Microsoft::Graphics::Canvas::Text::CanvasTextLayout^ scales,
            Microsoft::Graphics::Canvas::Text::CanvasTextLayout^ cscale,
			Microsoft::Graphics::Canvas::Brushes::ICanvasBrush^ color);

    private:
		Microsoft::Graphics::Canvas::Text::CanvasTextFormat^ label_font;
		Microsoft::Graphics::Canvas::Geometry::CanvasCachedGeometry^ scale_marks;
		Microsoft::Graphics::Canvas::Text::CanvasTextLayout^ caption;
		Microsoft::Graphics::Canvas::Text::CanvasTextLayout^ scales;
		Microsoft::Graphics::Canvas::Text::CanvasTextLayout^ cscale;
		Microsoft::Graphics::Canvas::Brushes::ICanvasBrush^ color;
        unsigned int range;
        unsigned char step;

    private:
		float ch; 
		float width;
        float height;
        float meter_width;
        float mark_interval;
    };
}
