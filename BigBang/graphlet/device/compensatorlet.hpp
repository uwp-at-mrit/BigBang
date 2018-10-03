#pragma once

#include "graphlet/primitive.hpp"

#include "paint.hpp"
#include "brushes.hxx"

namespace WarGrey::SCADA {
	private class Compensatorlet : public WarGrey::SCADA::IRangelet<double> {
	public:
		Compensatorlet(double range, float width, float height = 0.0F,
			unsigned int step = 0U, unsigned int precision = 1U,
			Microsoft::Graphics::Canvas::Brushes::ICanvasBrush^ color = nullptr,
			Microsoft::Graphics::Canvas::Brushes::ICanvasBrush^ progress_color = nullptr);

	public:
		void construct() override;
		void fill_extent(float x, float y, float* w = nullptr, float* h = nullptr) override;
		void draw(Microsoft::Graphics::Canvas::CanvasDrawingSession^ ds, float x, float y, float Width, float Height) override;

	protected:
		void on_value_changed(double t) override;

	private:
		float get_outlet_height(double percentage);

	private:
		Microsoft::Graphics::Canvas::Geometry::CanvasCachedGeometry^ progress;
		Microsoft::Graphics::Canvas::Geometry::CanvasGeometry^ base;
		Microsoft::Graphics::Canvas::Geometry::CanvasGeometry^ pulley;
		Microsoft::Graphics::Canvas::Brushes::ICanvasBrush^ color;
		Microsoft::Graphics::Canvas::Brushes::ICanvasBrush^ progress_color;

	private:
		float pulley_size;
		float base_width;
		float base_height;
		float progress_width;
		float anchor_py;
		float anchor_by;

	private:
		float width;
		float height;
		float thickness;

	private:
		unsigned int step;
		unsigned int precision;
	};
}
