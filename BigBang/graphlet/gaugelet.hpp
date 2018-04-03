#pragma once

#include "graphlet/primitive.hpp"
#include "brushes.hxx"

namespace WarGrey::SCADA {
	private class IGaugelet abstract : public WarGrey::SCADA::IScalelet<float> {
	public:
		IGaugelet(float vmin, float vmax, float meter_width_ratio, unsigned char step,
			Microsoft::Graphics::Canvas::Brushes::ICanvasBrush^ color = WarGrey::SCADA::Colours::Green,
			Microsoft::Graphics::Canvas::Brushes::ICanvasBrush^ body_color = WarGrey::SCADA::Colours::DarkGray,
			Microsoft::Graphics::Canvas::Brushes::ICanvasBrush^ mark_color = WarGrey::SCADA::Colours::GhostWhite);

	public:
		void fill_extent(float x, float y, float* w = nullptr, float* h = nullptr) override;
		void draw(Microsoft::Graphics::Canvas::CanvasDrawingSession^ ds, float x, float y, float Width, float Height) override;

	private:
		Microsoft::Graphics::Canvas::Geometry::CanvasCachedGeometry^ scale_marks;
		Microsoft::Graphics::Canvas::Brushes::ICanvasBrush^ color;
		Microsoft::Graphics::Canvas::Brushes::ICanvasBrush^ body_color;
		Microsoft::Graphics::Canvas::Brushes::ICanvasBrush^ mark_color;

	protected:
		float min_value;
		float max_value;

	private:
		float height;
		float body_x;
		float body_y;
		float body_width;
		float body_height;
	};
	
	private class LiquidGaugelet : public WarGrey::SCADA::IGaugelet {
    public:
		LiquidGaugelet(float range, float meter_width_ratio, unsigned char step = 0,
			Microsoft::Graphics::Canvas::Brushes::ICanvasBrush^ color = WarGrey::SCADA::Colours::Green,
			Microsoft::Graphics::Canvas::Brushes::ICanvasBrush^ body_color = WarGrey::SCADA::Colours::DarkGray,
			Microsoft::Graphics::Canvas::Brushes::ICanvasBrush^ mark_color = WarGrey::SCADA::Colours::GhostWhite);
    };
}
