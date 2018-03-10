#pragma once

#include "snip/snip.hpp"
#include "turtle.hpp"

namespace WarGrey::SCADA {
	private class Shapelet : public WarGrey::SCADA::ISnip {};

	private class Geometrylet : public WarGrey::SCADA::Shapelet {
	public:
		Geometrylet(Microsoft::Graphics::Canvas::Geometry::CanvasGeometry^ shape, Windows::UI::Color& color,
			Windows::UI::Color& border_color = Windows::UI::Colors::Transparent,
			float thickness = 1.0F);

	public:
		void fill_extent(float x, float y, float* w = nullptr, float* h = nullptr) override;
		void draw(Microsoft::Graphics::Canvas::CanvasDrawingSession^ ds, float x, float y, float Width, float Height) override;

	private:
		Microsoft::Graphics::Canvas::Geometry::CanvasCachedGeometry^ surface;
		Microsoft::Graphics::Canvas::Geometry::CanvasCachedGeometry^ border;
		Microsoft::Graphics::Canvas::Brushes::ICanvasBrush^ border_color;
		Microsoft::Graphics::Canvas::Brushes::ICanvasBrush^ color;

	protected:
		Windows::Foundation::Rect box;
	};

	private class Tracklet : public WarGrey::SCADA::Geometrylet {
	public:
		~Tracklet() noexcept;

		Tracklet(WarGrey::SCADA::Turtle* turtle, float thickness = 1.0F,
			Windows::UI::Color& color = Windows::UI::Colors::Azure);

	public:
		void fill_anchor_location(int node, float* x, float* y);

	private:
		WarGrey::SCADA::Turtle* turtle;
		float thickness;
	};
}
