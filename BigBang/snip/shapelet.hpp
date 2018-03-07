#pragma once

#include "snip/snip.hpp"
#include "shape.hpp"

namespace WarGrey::SCADA {
	private class Shapelet : public WarGrey::SCADA::ISnip {
	public:
		Shapelet(Microsoft::Graphics::Canvas::Geometry::CanvasGeometry^ shape, Windows::UI::Color& color,
			Windows::UI::Color& border_color = Windows::UI::Colors::Transparent,
			float thickness = 1.0F);

	public:
		void fill_extent(float x, float y, float* w = nullptr, float* h = nullptr) override;
		void draw(Microsoft::Graphics::Canvas::CanvasDrawingSession^ ds, float x, float y, float Width, float Height) override;

	public:
		void fill_extent_offset(float* x = nullptr, float* y = nullptr);

	private:
		Microsoft::Graphics::Canvas::Geometry::CanvasCachedGeometry^ surface;
		Microsoft::Graphics::Canvas::Geometry::CanvasCachedGeometry^ border;
		Microsoft::Graphics::Canvas::Brushes::ICanvasBrush^ border_color;
		Microsoft::Graphics::Canvas::Brushes::ICanvasBrush^ color;

	private:
		Windows::Foundation::Rect box;
	};

	private class Pipelinelet : public WarGrey::SCADA::Shapelet {
	public:
		Pipelinelet(Platform::Array<TurtleMove>^ moves, float unit_length = 16.0F, float thickness = 1.0F,
			Windows::UI::Color& color = Windows::UI::Colors::Azure);
	};
}
