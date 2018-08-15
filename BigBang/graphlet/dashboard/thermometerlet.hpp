#pragma once

#include "graphlet/primitive.hpp"

#include "paint.hpp"
#include "brushes.hxx"

namespace WarGrey::SCADA {
	private class Thermometerlet : public WarGrey::SCADA::IRangelet<float> {
	public:
		Thermometerlet(float tmin, float tmax, unsigned int step,
			float width, float height = 0.0F, float thickness = 3.0F,
			Microsoft::Graphics::Canvas::Brushes::ICanvasBrush^ border_color = nullptr,
			WarGrey::SCADA::GradientStops^ colors = nullptr);

		Thermometerlet(WarGrey::SCADA::FitPosition mark_position,
			float tmin, float tmax, unsigned int step, float width, float height = 0.0F, float thickness = 3.0F,
			Microsoft::Graphics::Canvas::Brushes::ICanvasBrush^ border_color = nullptr,
			WarGrey::SCADA::GradientStops^ colors = nullptr);

		Thermometerlet(float range, unsigned int step, float width,
			float height = 0.0F, float thickness = 3.0F,
			Microsoft::Graphics::Canvas::Brushes::ICanvasBrush^ border_color = nullptr,
			WarGrey::SCADA::GradientStops^ colors = nullptr);

		Thermometerlet(WarGrey::SCADA::FitPosition mark_position, float range, unsigned int step,
			float width, float height = 0.0F, float thickness = 3.0F,
			Microsoft::Graphics::Canvas::Brushes::ICanvasBrush^ border_color = nullptr,
			WarGrey::SCADA::GradientStops^ colors = nullptr);

		Thermometerlet(float width, float height = 0.0F, float thickness = 3.0F,
			Microsoft::Graphics::Canvas::Brushes::ICanvasBrush^ border_color = nullptr,
			WarGrey::SCADA::GradientStops^ colors = nullptr);

		Thermometerlet(WarGrey::SCADA::FitPosition mark_position,
			float width, float height = 0.0F, float thickness = 3.0F,
			Microsoft::Graphics::Canvas::Brushes::ICanvasBrush^ border_color = nullptr,
			WarGrey::SCADA::GradientStops^ colors = nullptr);

	public:
		void construct() override;
		void fill_extent(float x, float y, float* w = nullptr, float* h = nullptr) override;
		void draw(Microsoft::Graphics::Canvas::CanvasDrawingSession^ ds, float x, float y, float Width, float Height) override;

	public:
		void fill_mercury_extent(float* x = nullptr, float* y = nullptr, float* width = nullptr, float* height = nullptr);
		void fill_mercury_extent(float percentage, float* x = nullptr, float* y = nullptr, float* width = nullptr, float* height = nullptr);

	protected:
		void on_value_changed(float t) override;

	private:
		WarGrey::SCADA::GradientStops^ colors;
		Microsoft::Graphics::Canvas::Geometry::CanvasCachedGeometry^ skeleton;
		Microsoft::Graphics::Canvas::Geometry::CanvasCachedGeometry^ mercury;
		Microsoft::Graphics::Canvas::Brushes::ICanvasBrush^ border_color;
		Microsoft::Graphics::Canvas::Brushes::ICanvasBrush^ mercury_color;

	private:
		float bulb_size;
		float width;
		float height;
		float thickness;
		float mercury_x;
		float mercury_y;
		
	private:
		WarGrey::SCADA::FitPosition mark_position;

	private:
		unsigned int step;
	};
}
