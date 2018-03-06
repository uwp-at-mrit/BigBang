#pragma once

#include "snip/snip.hpp"
#include "shape.hpp"

namespace WarGrey::SCADA {
	private class Pipelinelet : public WarGrey::SCADA::ISnip {
	public:
		Pipelinelet(Platform::Array<PipeMove>^ moves, float unit_length = 16.0F, float thickness = 1.0F,
			Windows::UI::Color& color = Windows::UI::Colors::Azure);

		Pipelinelet(float x, float y, Platform::Array<PipeMove>^ moves,
			float unit_length = 16.0F, float thickness = 1.0F,
			Windows::UI::Color& color = Windows::UI::Colors::Azure);

	public:
		void fill_extent(float x, float y, float* w = nullptr, float* h = nullptr) override;
		void draw(Microsoft::Graphics::Canvas::CanvasDrawingSession^ ds, float x, float y, float Width, float Height) override;

	private:
		Microsoft::Graphics::Canvas::Geometry::CanvasCachedGeometry^ pipeline;
		Microsoft::Graphics::Canvas::Brushes::ICanvasBrush^ color;

	private:
		float thickness;
		float width;
		float height;
	};
}
