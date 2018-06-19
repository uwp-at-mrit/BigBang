#pragma once

#include "graphlet/primitive.hpp"

#include "paint.hpp"
#include "brushes.hxx"

namespace WarGrey::SCADA {
	private class Thermometerlet : public WarGrey::SCADA::IValuelet<float> {
	public:
		Thermometerlet(float tmin, float tmax, float width, float height = 0.0F,
			Microsoft::Graphics::Canvas::Brushes::ICanvasBrush^ border_color = WarGrey::SCADA::Colours::make(0x666666),
			GradientStops^ colors = nullptr);

		Thermometerlet(float width, float height = 0.0F,
			Microsoft::Graphics::Canvas::Brushes::ICanvasBrush^ border_color = WarGrey::SCADA::Colours::make(0x666666),
			GradientStops^ colors = nullptr);

	public:
		void construct() override;
		void fill_extent(float x, float y, float* w = nullptr, float* h = nullptr) override;
		void draw(Microsoft::Graphics::Canvas::CanvasDrawingSession^ ds, float x, float y, float Width, float Height) override;

	private:
		Microsoft::Graphics::Canvas::Geometry::CanvasCachedGeometry^ skeleton;
		Microsoft::Graphics::Canvas::Brushes::ICanvasBrush^ border_color;
		Microsoft::Graphics::Canvas::Brushes::CanvasLinearGradientBrush^ mercury_color;

	private:
		float vmin;
		float vmax;
		float bulb_width;
		float width;
		float height;
		float thickness;
	};
}
