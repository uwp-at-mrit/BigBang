#pragma once

#include "graphlet/primitive.hpp"

#include "paint.hpp"
#include "brushes.hxx"

namespace WarGrey::SCADA {
	private class DensityFlowmeterlet : public WarGrey::SCADA::IGraphlet {
	public:
		DensityFlowmeterlet(double density_min, double density_max, double flspeed_min, double flspeed_max,
			float radius, double degrees_offset = 4.0, float thickness = 2.0F,
			unsigned int step = 0U, unsigned int precision = 1U,
			Microsoft::Graphics::Canvas::Brushes::ICanvasBrush^ pointer_color = nullptr,
			Microsoft::Graphics::Canvas::Brushes::ICanvasBrush^ density_color = nullptr,
			Microsoft::Graphics::Canvas::Brushes::ICanvasBrush^ flspeed_color = nullptr,
			Microsoft::Graphics::Canvas::Brushes::ICanvasBrush^ base_color = nullptr);

		DensityFlowmeterlet(float radius, double degrees_offset = 4.0, float thickness = 2.0F,
			unsigned int step = 0U, unsigned int precision = 1U,
			Microsoft::Graphics::Canvas::Brushes::ICanvasBrush^ pointer_color = nullptr,
			Microsoft::Graphics::Canvas::Brushes::ICanvasBrush^ density_color = nullptr,
			Microsoft::Graphics::Canvas::Brushes::ICanvasBrush^ flspeed_color = nullptr,
			Microsoft::Graphics::Canvas::Brushes::ICanvasBrush^ base_color = nullptr);

	public:
		void construct() override;
		void fill_extent(float x, float y, float* w = nullptr, float* h = nullptr) override;
		void draw(Microsoft::Graphics::Canvas::CanvasDrawingSession^ ds, float x, float y, float Width, float Height) override;

	public:
		void set_values(double density, double flspeed);
		double get_density() { return this->_density; }
		double get_flspeed() { return this->_flspeed; }

	private:
		WarGrey::SCADA::GradientStops^ colors;
		Microsoft::Graphics::Canvas::Geometry::CanvasCachedGeometry^ skeleton;
		Microsoft::Graphics::Canvas::Geometry::CanvasCachedGeometry^ density;
		Microsoft::Graphics::Canvas::Geometry::CanvasCachedGeometry^ flspeed;
		Microsoft::Graphics::Canvas::Geometry::CanvasCachedGeometry^ pointers;
		Microsoft::Graphics::Canvas::Geometry::CanvasCachedGeometry^ endpoint_bases;
		Microsoft::Graphics::Canvas::Geometry::CanvasCachedGeometry^ endpoints;
		Microsoft::Graphics::Canvas::Brushes::ICanvasBrush^ pointer_color;
		Microsoft::Graphics::Canvas::Brushes::ICanvasBrush^ density_color;
		Microsoft::Graphics::Canvas::Brushes::ICanvasBrush^ flspeed_color;
		Microsoft::Graphics::Canvas::Brushes::ICanvasBrush^ base_color;

	private:
		float width;
		float height;
		float radius;
		float pointer_radius;
		float depcx;
		float fepcx;
		float epcy;
		float thickness;

	private:
		double density_min;
		double density_max;
		double density_start;
		double density_end;
		double flspeed_min;
		double flspeed_max;
		double flspeed_start;
		double flspeed_end;
		unsigned int step;
		unsigned int precision;

	private:
		double _density;
		double _flspeed;
	};
}
