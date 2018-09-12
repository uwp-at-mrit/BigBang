#pragma once

#include "graphlet/primitive.hpp"

#include "paint.hpp"

namespace WarGrey::SCADA {
	private class Hatchlet : public WarGrey::SCADA::IGraphlet {
	public:
		Hatchlet(float width, float height = 0.0F, float thickness = 2.0F,
			Microsoft::Graphics::Canvas::Brushes::CanvasSolidColorBrush^ color = nullptr);

	public:
		void construct() override;
		void fill_extent(float x, float y, float* w = nullptr, float* h = nullptr) override;
		void draw(Microsoft::Graphics::Canvas::CanvasDrawingSession^ ds, float x, float y, float Width, float Height) override;

	private:
		Microsoft::Graphics::Canvas::Geometry::CanvasCachedGeometry^ wheel_border;
		Microsoft::Graphics::Canvas::Geometry::CanvasCachedGeometry^ wheel_handler;
		Microsoft::Graphics::Canvas::Geometry::CanvasGeometry^ border;
		Microsoft::Graphics::Canvas::Brushes::ICanvasBrush^ color;
		
	private:
		float width;
		float height;
		float thickness;
	};
}
