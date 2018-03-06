#pragma once

#include "snip/snip.hpp"

namespace WarGrey::SCADA {
	private class Pumplet : public WarGrey::SCADA::ISnip {
	public:
		Pumplet(float radius, double degree = -90.0, float thickness = 1.0F,
			Windows::UI::Color& color = Windows::UI::Colors::Gray,
			Windows::UI::Color& ring_color = Windows::UI::Colors::WhiteSmoke);

	public:
		void fill_extent(float x, float y, float* w = nullptr, float* h = nullptr) override;
		void draw(Microsoft::Graphics::Canvas::CanvasDrawingSession^ ds, float x, float y, float Width, float Height) override;

	private:
		Microsoft::Graphics::Canvas::Geometry::CanvasCachedGeometry^ triangle;
		Microsoft::Graphics::Canvas::Brushes::ICanvasBrush^ color;
		Microsoft::Graphics::Canvas::Brushes::ICanvasBrush^ ring_color;

	private:
		float radius;
		float thickness;
	};
}
