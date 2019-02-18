#pragma once

#include "graphlet/primitive.hpp"

#include "paint.hpp"
#include "brushes.hxx"

namespace WarGrey::SCADA {
	private class DensitySpeedmeterlet : public WarGrey::SCADA::IGraphlet {
	public:
		DensitySpeedmeterlet(double density_min, double density_max, double flspeed_min, double flspeed_max,
			float radius, double degrees_offset = 4.0, float thickness = 2.0F,
			unsigned int step = 0U, unsigned int precision = 1U,
			Microsoft::Graphics::Canvas::Brushes::ICanvasBrush^ pointer_color = nullptr,
			Microsoft::Graphics::Canvas::Brushes::ICanvasBrush^ density_color = nullptr,
			Microsoft::Graphics::Canvas::Brushes::ICanvasBrush^ flspeed_color = nullptr,
			Microsoft::Graphics::Canvas::Brushes::ICanvasBrush^ base_color = nullptr);

		DensitySpeedmeterlet(float radius, double degrees_offset = 4.0, float thickness = 2.0F,
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
		void set_values(double density, double flspeed, bool force = false);
		void set_density(double density, bool force = false);
		void set_flspeed(double density, bool force = false);
		double get_density() { return this->_density; }
		double get_flspeed() { return this->_flspeed; }

	private:
		Microsoft::Graphics::Canvas::Geometry::CanvasCachedGeometry^ density;
		Microsoft::Graphics::Canvas::Geometry::CanvasCachedGeometry^ flspeed;
		Microsoft::Graphics::Canvas::Geometry::CanvasCachedGeometry^ endpoint_bases;
		Microsoft::Graphics::Canvas::Geometry::CanvasCachedGeometry^ endpoints;
		Microsoft::Graphics::Canvas::Geometry::CanvasCachedGeometry^ density_pointer;
		Microsoft::Graphics::Canvas::Geometry::CanvasCachedGeometry^ flspeed_pointer;
		Microsoft::Graphics::Canvas::Brushes::ICanvasBrush^ pointer_color;
		Microsoft::Graphics::Canvas::Brushes::ICanvasBrush^ density_color;
		Microsoft::Graphics::Canvas::Brushes::ICanvasBrush^ flspeed_color;
		Microsoft::Graphics::Canvas::Brushes::ICanvasBrush^ base_color;

	private:
		Microsoft::Graphics::Canvas::Text::CanvasTextFormat^ value_font;
		Microsoft::Graphics::Canvas::Text::CanvasTextLayout^ density_value;
		Microsoft::Graphics::Canvas::Text::CanvasTextLayout^ flspeed_value;
		Microsoft::Graphics::Canvas::Geometry::CanvasCachedGeometry^ density_unit;
		Microsoft::Graphics::Canvas::Geometry::CanvasCachedGeometry^ flspeed_unit;
		float label_rx;
		float label_by;

	private:
		float width;
		float height;
		float radius;
		float dxoff;
		float fxoff;
		float epcxoff;
		float yoff;
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
