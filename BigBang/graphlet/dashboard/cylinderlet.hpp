#pragma once

#include "graphlet/primitive.hpp"

#include "paint.hpp"
#include "brushes.hxx"

namespace WarGrey::SCADA {
	private enum class LiquidSurface { Convex, Concave, _ };
	
	private class Cylinderlet : public WarGrey::SCADA::IRangelet<double> {
	public:
		Cylinderlet(WarGrey::SCADA::LiquidSurface liquid_shape,
			double vmin, double vmax, float base_width, float height = 0.0F,
			float thickness = 3.0F, unsigned int step = 0U, unsigned int precision = 1U,
			Microsoft::Graphics::Canvas::Brushes::CanvasSolidColorBrush^ border_color = nullptr,
			WarGrey::SCADA::GradientStops^ colors = nullptr);

		Cylinderlet(WarGrey::SCADA::LiquidSurface liquid_shape,
			double range, float base_width, float height = 0.0F, float thickness = 3.0F,
			unsigned int step = 0U, unsigned int precision = 1U,
			Microsoft::Graphics::Canvas::Brushes::CanvasSolidColorBrush^ border_color = nullptr,
			WarGrey::SCADA::GradientStops^ colors = nullptr);

		Cylinderlet(double range, float base_width, float height = 0.0F, float thickness = 3.0F,
			unsigned int step = 0U, unsigned int precision = 1U,
			Microsoft::Graphics::Canvas::Brushes::CanvasSolidColorBrush^ border_color = nullptr,
			WarGrey::SCADA::GradientStops^ colors = nullptr);

		Cylinderlet(double vmin, double vmax, float base_width, float height = 0.0F,
			float thickness = 3.0F, unsigned int step = 0U, unsigned int precision = 1U,
			Microsoft::Graphics::Canvas::Brushes::CanvasSolidColorBrush^ border_color = nullptr,
			WarGrey::SCADA::GradientStops^ colors = nullptr);

	public:
		void construct() override;
		void fill_extent(float x, float y, float* w = nullptr, float* h = nullptr) override;
		void draw(Microsoft::Graphics::Canvas::CanvasDrawingSession^ ds, float x, float y, float Width, float Height) override;

	protected:
		void on_value_changed(double t) override;

	private:
		Microsoft::Graphics::Canvas::Geometry::CanvasGeometry^ make_liquid_shape(double percentage);

	private:
		Microsoft::Graphics::Canvas::Geometry::CanvasCachedGeometry^ skeleton;
		Microsoft::Graphics::Canvas::Geometry::CanvasCachedGeometry^ liquid;
		Microsoft::Graphics::Canvas::Geometry::CanvasGeometry^ body;
		Microsoft::Graphics::Canvas::Brushes::ICanvasBrush^ border_color;
		Microsoft::Graphics::Canvas::Brushes::ICanvasBrush^ color;
		WarGrey::SCADA::GradientStops^ colors;

	private:
		float base_width;
		float extended_width;
		float height;
		float thickness;
		float liquid_surface_radius;

	private:
		WarGrey::SCADA::LiquidSurface liquid_shape;
		bool leftward;

	private:
		unsigned int step;
		unsigned int precision;
	};
}
