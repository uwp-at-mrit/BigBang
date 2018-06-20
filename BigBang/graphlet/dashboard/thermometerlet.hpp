#pragma once

#include "graphlet/primitive.hpp"

#include "paint.hpp"
#include "brushes.hxx"

namespace WarGrey::SCADA {
	private class Thermometerlet : public WarGrey::SCADA::IRangelet<float> {
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

	public:
		void fill_mercury_extent(float* x = nullptr, float* y = nullptr, float* width = nullptr, float* height = nullptr);
		void fill_mercury_extent(float percentage, float* x = nullptr, float* y = nullptr, float* width = nullptr, float* height = nullptr);

	protected:
		void on_value_change(float t) override;

	private:
		GradientStops^ color_stops;
		Microsoft::Graphics::Canvas::Geometry::CanvasCachedGeometry^ skeleton;
		Microsoft::Graphics::Canvas::Geometry::CanvasCachedGeometry^ mercury;
		Microsoft::Graphics::Canvas::Geometry::CanvasCachedGeometry^ mercury_bulb;
		Microsoft::Graphics::Canvas::Brushes::ICanvasBrush^ border_color;
		Microsoft::Graphics::Canvas::Brushes::ICanvasBrush^ bulb_color;
		Microsoft::Graphics::Canvas::Brushes::CanvasLinearGradientBrush^ mercury_color;

	private:
		float bulb_size;
		float width;
		float height;
		float thickness;
		float mercury_x;
		float mercury_y;
		float bulb_x;
		float bulb_y;
	};
}
