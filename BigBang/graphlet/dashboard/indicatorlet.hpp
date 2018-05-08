#pragma once

#include "graphlet/primitive.hpp"
#include "brushes.hxx"

namespace WarGrey::SCADA {
	private class Indicatorlet : public WarGrey::SCADA::IValuelet<float> {
	public:
		Indicatorlet(float size, float thickness = 0.0F,
			Microsoft::Graphics::Canvas::Brushes::ICanvasBrush^ color = WarGrey::SCADA::Colours::Black,
			Microsoft::Graphics::Canvas::Brushes::ICanvasBrush^ low_color = WarGrey::SCADA::Colours::make(0x30A1F6),
			Microsoft::Graphics::Canvas::Brushes::ICanvasBrush^ normal_color = WarGrey::SCADA::Colours::make(0xAEEE00),
			Microsoft::Graphics::Canvas::Brushes::ICanvasBrush^ high_color = WarGrey::SCADA::Colours::make(0xFFB43D));

	public:
		void construct() override;
		void fill_extent(float x, float y, float* w = nullptr, float* h = nullptr) override;
		void draw(Microsoft::Graphics::Canvas::CanvasDrawingSession^ ds, float x, float y, float Width, float Height) override;

	private:
		Microsoft::Graphics::Canvas::Geometry::CanvasCachedGeometry^ skeleton;
		Microsoft::Graphics::Canvas::Brushes::ICanvasBrush^ color;
		Microsoft::Graphics::Canvas::Brushes::ICanvasBrush^ normal_color;
		Microsoft::Graphics::Canvas::Brushes::ICanvasBrush^ low_color;
		Microsoft::Graphics::Canvas::Brushes::ICanvasBrush^ high_color;

	private:
		Windows::Foundation::Rect temperature;
		float bulb_width;
		float size;
		float thickness;
	};
}
