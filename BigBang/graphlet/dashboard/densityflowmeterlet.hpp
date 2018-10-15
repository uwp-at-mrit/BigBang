#pragma once

#include "graphlet/primitive.hpp"

#include "paint.hpp"
#include "brushes.hxx"

namespace WarGrey::SCADA {
	private class DensityFlowmeterlet : public WarGrey::SCADA::IGraphlet {
	public:
		DensityFlowmeterlet(double dmin, double dmax, double fmin, double fmax, float width, float height = 0.0F,
			float thickness = 2.0F, unsigned int step = 0U,
			Microsoft::Graphics::Canvas::Brushes::ICanvasBrush^ border_color = nullptr,
			WarGrey::SCADA::GradientStops^ colors = nullptr);

		DensityFlowmeterlet(float width, float height = 0.0F, float thickness = 2.0F, unsigned int step = 0U,
			Microsoft::Graphics::Canvas::Brushes::ICanvasBrush^ border_color = nullptr,
			WarGrey::SCADA::GradientStops^ colors = nullptr);

	public:
		void construct() override;
		void fill_extent(float x, float y, float* w = nullptr, float* h = nullptr) override;
		void draw(Microsoft::Graphics::Canvas::CanvasDrawingSession^ ds, float x, float y, float Width, float Height) override;

	protected:
		//void on_value_changed(double t) override;

	private:
		WarGrey::SCADA::GradientStops^ colors;
		Microsoft::Graphics::Canvas::Geometry::CanvasCachedGeometry^ skeleton;
		Microsoft::Graphics::Canvas::Geometry::CanvasCachedGeometry^ density;
		Microsoft::Graphics::Canvas::Geometry::CanvasCachedGeometry^ flspeed;
		Microsoft::Graphics::Canvas::Brushes::ICanvasBrush^ border_color;
		Microsoft::Graphics::Canvas::Brushes::ICanvasBrush^ mercury_color;

	private:
		float width;
		float height;
		float thickness;

	private:
		double density_min;
		double density_max;
		double flspeed_min;
		double flspeed_max;
		unsigned int step;
	};
}
