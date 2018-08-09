#pragma once

#include "graphlet/primitive.hpp"

#include "paint.hpp"
#include "brushes.hxx"

namespace WarGrey::SCADA {
	private class ICylinderlet abstract : public WarGrey::SCADA::IRangelet<float> {
	public:
		ICylinderlet(float vmin, float vmax, float width, float height, unsigned int step,
			WarGrey::SCADA::GradientStops^ colors,
			Microsoft::Graphics::Canvas::Brushes::CanvasSolidColorBrush^ border_color);

	public:
		void construct() override;
		void fill_extent(float x, float y, float* w = nullptr, float* h = nullptr) override;
		void draw(Microsoft::Graphics::Canvas::CanvasDrawingSession^ ds, float x, float y, float Width, float Height) override;

	protected:
		virtual Microsoft::Graphics::Canvas::Geometry::CanvasGeometry^ make_liquid_shape(
			Microsoft::Graphics::Canvas::Geometry::CanvasGeometry^ body,
			float percentage,
			float surface_radius) = 0;

	protected:
		void on_value_changed(float t) override;

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
	};

	private class Cylinderlet : public WarGrey::SCADA::ICylinderlet {
	public:
		Cylinderlet(float range, float width, float height, unsigned int step = 0,
			WarGrey::SCADA::GradientStops^ colors = nullptr,
			Microsoft::Graphics::Canvas::Brushes::CanvasSolidColorBrush^ border_color = WarGrey::SCADA::Colours::make(0xBBBBBB));

		Cylinderlet(float tmin, float tmax, float width, float height, unsigned int step = 0,
			WarGrey::SCADA::GradientStops^ colors = nullptr,
			Microsoft::Graphics::Canvas::Brushes::CanvasSolidColorBrush^ border_color = WarGrey::SCADA::Colours::make(0xBBBBBB));

	protected:
		Microsoft::Graphics::Canvas::Geometry::CanvasGeometry^ make_liquid_shape(
			Microsoft::Graphics::Canvas::Geometry::CanvasGeometry^ body,
			float percentage,
			float surface_radius) override;
	};

	private class ConvexCylinderlet : public WarGrey::SCADA::ICylinderlet {
	public:
		ConvexCylinderlet(float range, float width, float height, unsigned int step = 0,
			WarGrey::SCADA::GradientStops^ colors = nullptr,
			Microsoft::Graphics::Canvas::Brushes::CanvasSolidColorBrush^ border_color = WarGrey::SCADA::Colours::make(0xBBBBBB));

		ConvexCylinderlet(float vmin, float vmax, float width, float height, unsigned int step = 0,
			WarGrey::SCADA::GradientStops^ colors = nullptr,
			Microsoft::Graphics::Canvas::Brushes::CanvasSolidColorBrush^ border_color = WarGrey::SCADA::Colours::make(0xBBBBBB));

	protected:
		Microsoft::Graphics::Canvas::Geometry::CanvasGeometry^ make_liquid_shape(
			Microsoft::Graphics::Canvas::Geometry::CanvasGeometry^ body,
			float percentage,
			float surface_radius) override;
	};

	private class ConcaveCylinderlet : public WarGrey::SCADA::ICylinderlet {
	public:
		ConcaveCylinderlet(float range, float width, float height, unsigned int step = 0,
			WarGrey::SCADA::GradientStops^ colors = nullptr,
			Microsoft::Graphics::Canvas::Brushes::CanvasSolidColorBrush^ border_color = WarGrey::SCADA::Colours::make(0xBBBBBB));

		ConcaveCylinderlet(float tmin, float tmax, float width, float height, unsigned int step = 0,
			WarGrey::SCADA::GradientStops^ colors = nullptr,
			Microsoft::Graphics::Canvas::Brushes::CanvasSolidColorBrush^ border_color = WarGrey::SCADA::Colours::make(0xBBBBBB));

	protected:
		Microsoft::Graphics::Canvas::Geometry::CanvasGeometry^ make_liquid_shape(
			Microsoft::Graphics::Canvas::Geometry::CanvasGeometry^ body,
			float percentage,
			float surface_radius) override;
	};
}
