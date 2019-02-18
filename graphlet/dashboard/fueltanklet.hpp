#pragma once

#include "graphlet/primitive.hpp"
#include "paint.hpp"
#include "brushes.hxx"

namespace WarGrey::SCADA {
	private class FuelTanklet : public WarGrey::SCADA::IRangelet<double> {
	public:
		FuelTanklet(float width, float height = 0.0F, float thickness = 3.0F,
			Microsoft::Graphics::Canvas::Brushes::ICanvasBrush^ border_color = nullptr,
			GradientStops^ stops = nullptr);

		FuelTanklet(double range, float width, float height = 0.0F, float thickness = 3.0F,
			Microsoft::Graphics::Canvas::Brushes::ICanvasBrush^ border_color = nullptr,
			GradientStops^ stops = nullptr);

		FuelTanklet(double vmin, double vmax, float width, float height = 0.0F, float thickness = 3.0F,
			Microsoft::Graphics::Canvas::Brushes::ICanvasBrush^ border_color = nullptr,
			GradientStops^ stops = nullptr);

	public:
		void construct() override;
		void fill_extent(float x, float y, float* w = nullptr, float* h = nullptr) override;
		void draw(Microsoft::Graphics::Canvas::CanvasDrawingSession^ ds, float x, float y, float Width, float Height) override;

	protected:
		void on_value_changed(double v) override;

	private:
		GradientStops^ colors;
		Microsoft::Graphics::Canvas::Geometry::CanvasCachedGeometry^ skeleton;
		Microsoft::Graphics::Canvas::Brushes::ICanvasBrush^ border_color;
		Microsoft::Graphics::Canvas::Brushes::ICanvasBrush^ fuel_color;

	private:
		Windows::Foundation::Rect fuel;
		float width;
		float height;
		float thickness;
	};
}
