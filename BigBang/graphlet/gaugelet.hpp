#pragma once

#include "graphlet/primitive.hpp"
#include "brushes.hxx"

namespace WarGrey::SCADA {
	private class IGaugelet : public WarGrey::SCADA::IScalelet<float> {
	public:
		IGaugelet(float range, float meter_width_ratio = 5.0F, unsigned char step = 0,
			Microsoft::Graphics::Canvas::Brushes::ICanvasBrush^ color = WarGrey::SCADA::Colours::Green,
			Microsoft::Graphics::Canvas::Brushes::ICanvasBrush^ body_color = WarGrey::SCADA::Colours::DarkGray,
			Microsoft::Graphics::Canvas::Brushes::ICanvasBrush^ mark_color = WarGrey::SCADA::Colours::GhostWhite);

	public:
		void construct() override;
		void fill_extent(float x, float y, float* w = nullptr, float* h = nullptr) override;
		void draw(Microsoft::Graphics::Canvas::CanvasDrawingSession^ ds, float x, float y, float Width, float Height) override;

	protected:
		void on_scale_change(float scale) override;

	private:
		Microsoft::Graphics::Canvas::Geometry::CanvasCachedGeometry^ scale_marks;
		Microsoft::Graphics::Canvas::Brushes::ICanvasBrush^ color;
		Microsoft::Graphics::Canvas::Brushes::ICanvasBrush^ body_color;
		Microsoft::Graphics::Canvas::Brushes::ICanvasBrush^ mark_color;

	protected:
		float range;

	private:
		float height;
		float body_x;
		float body_y;
		float body_width;
		float body_height;
	};
	
	private class Gaugelet : public WarGrey::SCADA::IScalelet<float> {
    public:
		Gaugelet(Platform::String^ caption, int range, unsigned char step = 0,
			Microsoft::Graphics::Canvas::Brushes::ICanvasBrush^ color = WarGrey::SCADA::Colours::Green);

	public:
        void construct() override;
		void fill_extent(float x, float y, float* w = nullptr, float* h = nullptr) override;
		void draw(Microsoft::Graphics::Canvas::CanvasDrawingSession^ ds, float x, float y, float Width, float Height) override;

	public:
		void on_scale_change(float scale) override;

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
