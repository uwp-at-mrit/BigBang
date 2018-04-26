#pragma once

#include "graphlet/primitive.hpp"
#include "brushes.hxx"

namespace WarGrey::SCADA {
	private class FuelTank : public WarGrey::SCADA::IScalelet<float> {
	public:
		FuelTank(float width, float height,
			Microsoft::Graphics::Canvas::Brushes::ICanvasBrush^ border_color = WarGrey::SCADA::Colours::make(0xFDFDFD),
			Microsoft::Graphics::Canvas::Brushes::ICanvasBrush^ normal_color = WarGrey::SCADA::Colours::make(0xB4F100),
			Microsoft::Graphics::Canvas::Brushes::ICanvasBrush^ warning_color = WarGrey::SCADA::Colours::make(0xFFB33C),
			Microsoft::Graphics::Canvas::Brushes::ICanvasBrush^ emergency_color = WarGrey::SCADA::Colours::make(0xF00D0D));

	public:
		void construct() override;
		void fill_extent(float x, float y, float* w = nullptr, float* h = nullptr) override;
		void draw(Microsoft::Graphics::Canvas::CanvasDrawingSession^ ds, float x, float y, float Width, float Height) override;

	private:
		Microsoft::Graphics::Canvas::Geometry::CanvasCachedGeometry^ skeleton;
		Microsoft::Graphics::Canvas::Brushes::ICanvasBrush^ border_color;
		Microsoft::Graphics::Canvas::Brushes::ICanvasBrush^ normal_color;
		Microsoft::Graphics::Canvas::Brushes::ICanvasBrush^ warning_color;
		Microsoft::Graphics::Canvas::Brushes::ICanvasBrush^ emergency_color;

	private:
		Windows::Foundation::Rect fuel;
		float width;
		float height;
		float thickness;
	};
}
