#pragma once

#include "graphlet/primitive.hpp"

#include "paint.hpp"
#include "brushes.hxx"

namespace WarGrey::SCADA {
	private enum class LiquidSurface { Convex, Concave, _ };
	private enum class MarkPosition { Left, Right, _};

	private class Cylinderlet : public WarGrey::SCADA::IRangelet<float> {
	public:
		Cylinderlet(WarGrey::SCADA::LiquidSurface liquid_shape, WarGrey::SCADA::MarkPosition mark_position,
			float vmin, float vmax, float width, float height, unsigned int step = 0,
			WarGrey::SCADA::GradientStops^ colors = nullptr,
			Microsoft::Graphics::Canvas::Brushes::CanvasSolidColorBrush^ border_color = nullptr);

		Cylinderlet(WarGrey::SCADA::LiquidSurface liquid_shape, WarGrey::SCADA::MarkPosition mark_position,
			float range, float width, float height, unsigned int step = 0,
			WarGrey::SCADA::GradientStops^ colors = nullptr,
			Microsoft::Graphics::Canvas::Brushes::CanvasSolidColorBrush^ border_color = nullptr);

		Cylinderlet(float range, float width, float height, unsigned int step = 0,
			WarGrey::SCADA::GradientStops^ colors = nullptr,
			Microsoft::Graphics::Canvas::Brushes::CanvasSolidColorBrush^ border_color = nullptr);

		Cylinderlet(float vmin, float vmax, float width, float height, unsigned int step = 0,
			WarGrey::SCADA::GradientStops^ colors = nullptr,
			Microsoft::Graphics::Canvas::Brushes::CanvasSolidColorBrush^ border_color = nullptr);

		Cylinderlet(WarGrey::SCADA::LiquidSurface liquid_shape, float range, float width, float height, unsigned int step = 0,
			WarGrey::SCADA::GradientStops^ colors = nullptr,
			Microsoft::Graphics::Canvas::Brushes::CanvasSolidColorBrush^ border_color = nullptr);

		Cylinderlet(WarGrey::SCADA::LiquidSurface liquid_shape, float vmin, float vmax, float width, float height, unsigned int step = 0,
			WarGrey::SCADA::GradientStops^ colors = nullptr,
			Microsoft::Graphics::Canvas::Brushes::CanvasSolidColorBrush^ border_color = nullptr);

		Cylinderlet(WarGrey::SCADA::MarkPosition mark_position, float range, float width, float height, unsigned int step = 0,
			WarGrey::SCADA::GradientStops^ colors = nullptr,
			Microsoft::Graphics::Canvas::Brushes::CanvasSolidColorBrush^ border_color = nullptr);

		Cylinderlet(WarGrey::SCADA::MarkPosition mark_position, float vmin, float vmax, float width, float height, unsigned int step = 0,
			WarGrey::SCADA::GradientStops^ colors = nullptr,
			Microsoft::Graphics::Canvas::Brushes::CanvasSolidColorBrush^ border_color = nullptr);

	public:
		void construct() override;
		void fill_extent(float x, float y, float* w = nullptr, float* h = nullptr) override;
		void draw(Microsoft::Graphics::Canvas::CanvasDrawingSession^ ds, float x, float y, float Width, float Height) override;

	protected:
		void on_value_changed(float t) override;

	private:
		Microsoft::Graphics::Canvas::Geometry::CanvasGeometry^ make_liquid_shape(float percentage);

	private:
		Microsoft::Graphics::Canvas::Geometry::CanvasCachedGeometry^ skeleton;
		Microsoft::Graphics::Canvas::Geometry::CanvasCachedGeometry^ liquid;
		Microsoft::Graphics::Canvas::Geometry::CanvasCachedGeometry^ mark;
		Microsoft::Graphics::Canvas::Geometry::CanvasGeometry^ body;
		Microsoft::Graphics::Canvas::Brushes::ICanvasBrush^ border_color;
		Microsoft::Graphics::Canvas::Brushes::ICanvasBrush^ color;
		WarGrey::SCADA::GradientStops^ colors;

	private:
		unsigned int step;
		float width;
		float height;
		float thickness;
		float liquid_surface_radius;

	private:
		WarGrey::SCADA::LiquidSurface liquid_shape;
		WarGrey::SCADA::MarkPosition mark_position;
		float mark_x;
	};
}
