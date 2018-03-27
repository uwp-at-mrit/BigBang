#pragma once

#include "graphlet/primitive.hpp"

namespace WarGrey::SCADA {
	private class Valvelet : public WarGrey::SCADA::IGraphlet {
	public:
		Valvelet(float length, double degrees = -90.0, float thickness = 1.0F,
			Windows::UI::Color& color = Windows::UI::Colors::Cyan,
			Windows::UI::Color& border_color = Windows::UI::Colors::WhiteSmoke);

	public:
		void fill_extent(float x, float y, float* width = nullptr, float* height = nullptr) override;
		void draw(Microsoft::Graphics::Canvas::CanvasDrawingSession^ ds, float x, float y, float Width, float Height) override;

	public:
		double get_direction_degree();

	private:
		Microsoft::Graphics::Canvas::Geometry::CanvasCachedGeometry^ triangle;
		Microsoft::Graphics::Canvas::Brushes::ICanvasBrush^ color;
		Microsoft::Graphics::Canvas::Brushes::ICanvasBrush^ border_color;

	private:
		double degrees;
		float radius;
		float thickness;
	};
}
