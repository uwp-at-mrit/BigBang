#pragma once

#include "graphlet/primitive.hpp"
#include "brushes.hxx"

namespace WarGrey::SCADA {
	private class Thermometerlet : public WarGrey::SCADA::IValuelet<float> {
	public:
		Thermometerlet(float width, float height = 0.0F,
			Microsoft::Graphics::Canvas::Brushes::ICanvasBrush^ border_color = WarGrey::SCADA::Colours::make(0x666666),
			Microsoft::Graphics::Canvas::Brushes::ICanvasBrush^ low_color = WarGrey::SCADA::Colours::make(0x385BFE),
			Microsoft::Graphics::Canvas::Brushes::ICanvasBrush^ normal_color = WarGrey::SCADA::Colours::make(0xB3F000),
			Microsoft::Graphics::Canvas::Brushes::ICanvasBrush^ high_color = WarGrey::SCADA::Colours::make(0xFFB03A));

	public:
		void construct() override;
		void fill_extent(float x, float y, float* w = nullptr, float* h = nullptr) override;
		void draw(Microsoft::Graphics::Canvas::CanvasDrawingSession^ ds, float x, float y, float Width, float Height) override;

	private:
		Microsoft::Graphics::Canvas::Geometry::CanvasCachedGeometry^ skeleton;
		Microsoft::Graphics::Canvas::Brushes::ICanvasBrush^ border_color;
		Microsoft::Graphics::Canvas::Brushes::ICanvasBrush^ normal_color;
		Microsoft::Graphics::Canvas::Brushes::ICanvasBrush^ low_color;
		Microsoft::Graphics::Canvas::Brushes::ICanvasBrush^ high_color;

	private:
		Windows::Foundation::Rect temperature;
		float bulb_width;
		float width;
		float height;
		float thickness;
	};
}
