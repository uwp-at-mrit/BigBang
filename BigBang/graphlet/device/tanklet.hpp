#pragma once

#include "graphlet/primitive.hpp"

#include "paint.hpp"
#include "brushes.hxx"

namespace WarGrey::SCADA {
	private class Tanklet : public WarGrey::SCADA::IRangelet<float> {
	public:
		Tanklet(WarGrey::SCADA::FitPosition mark_position,
			float width, float height = 0.0F, float thickness = 3.0F,
			Microsoft::Graphics::Canvas::Brushes::CanvasSolidColorBrush^ border_color = nullptr,
			Microsoft::Graphics::Canvas::Brushes::CanvasSolidColorBrush^ color = nullptr,
			Microsoft::Graphics::Canvas::Brushes::CanvasSolidColorBrush^ heater_color = nullptr,
			WarGrey::SCADA::GradientStops^ colors = nullptr);

		Tanklet(float width, float height = 0.0F, float thickness = 3.0F,
			Microsoft::Graphics::Canvas::Brushes::CanvasSolidColorBrush^ border_color = nullptr,
			Microsoft::Graphics::Canvas::Brushes::CanvasSolidColorBrush^ color = nullptr,
			Microsoft::Graphics::Canvas::Brushes::CanvasSolidColorBrush^ heater_color = nullptr,
			WarGrey::SCADA::GradientStops^ colors = nullptr);

	public:
		void construct() override;
		void fill_extent(float x, float y, float* w = nullptr, float* h = nullptr) override;
		void draw(Microsoft::Graphics::Canvas::CanvasDrawingSession^ ds, float x, float y, float Width, float Height) override;

	protected:
		void on_value_changed(float t) override;

	private:
		Microsoft::Graphics::Canvas::Geometry::CanvasCachedGeometry^ heater;
		Microsoft::Graphics::Canvas::Geometry::CanvasGeometry^ body;
		Microsoft::Graphics::Canvas::Brushes::ICanvasBrush^ border_color;
		Microsoft::Graphics::Canvas::Brushes::ICanvasBrush^ fill_color;
		Microsoft::Graphics::Canvas::Brushes::ICanvasBrush^ heater_color;
		Microsoft::Graphics::Canvas::Brushes::ICanvasBrush^ color;
		WarGrey::SCADA::GradientStops^ colors;

	private:
		float width;
		float height;
		float thickness;

	private:
		WarGrey::SCADA::FitPosition mark_position;

	private:
		unsigned int step;
	};
}
