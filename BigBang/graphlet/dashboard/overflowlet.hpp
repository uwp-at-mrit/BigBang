#pragma once

#include "graphlet/primitive.hpp"

#include "paint.hpp"
#include "brushes.hxx"

namespace WarGrey::SCADA {
	private class OverflowPipelet : public WarGrey::SCADA::IRangelet<double> {
	public:
		OverflowPipelet(double range, float width, float height = 0.0F,
			unsigned int step = 0U, unsigned int precision = 1U,
			Microsoft::Graphics::Canvas::Brushes::ICanvasBrush^ color = nullptr,
			Microsoft::Graphics::Canvas::Brushes::ICanvasBrush^ liquid_color = nullptr,
			Microsoft::Graphics::Canvas::Brushes::CanvasSolidColorBrush^ hatch_color = nullptr);

	public:
		void construct() override;
		void fill_extent(float x, float y, float* w = nullptr, float* h = nullptr) override;
		void draw(Microsoft::Graphics::Canvas::CanvasDrawingSession^ ds, float x, float y, float Width, float Height) override;

	public:
		void set_liquid_height(double h, bool force_upadte = false);

	protected:
		void on_value_changed(double t) override;
		virtual void on_liquid_height_changed(double h);

	private:
		float get_outlet_height(double percentage);

	private:
		Microsoft::Graphics::Canvas::Geometry::CanvasCachedGeometry^ liquid;
		Microsoft::Graphics::Canvas::Geometry::CanvasGeometry^ hatchmark;
		Microsoft::Graphics::Canvas::Geometry::CanvasGeometry^ body;
		Microsoft::Graphics::Canvas::Brushes::ICanvasBrush^ color;
		Microsoft::Graphics::Canvas::Brushes::ICanvasBrush^ liquid_color;
		Microsoft::Graphics::Canvas::Brushes::ICanvasBrush^ hatch_color;

	private:
		double liquid_height;

	private:
		float ofbbase;
		float ofubase;
		float em;

	private:
		float width;
		float height;
		float thickness;

	private:
		unsigned int step;
		unsigned int precision;
	};
}
