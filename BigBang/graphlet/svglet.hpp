#pragma once

#include "graphlet/primitive.hpp"
#include "credit.hpp"
#include "turtle.hpp"
#include "brushes.hxx"

namespace WarGrey::SCADA {
	private class Svglet : public virtual WarGrey::SCADA::IGraphlet {
	public:
		Svglet(Platform::String^ file_svg, float width, float height, Platform::String^ root = "graphlet.svg");

	public:
		void fill_extent(float x, float y, float* w = nullptr, float* h = nullptr) override;
		void draw(Microsoft::Graphics::Canvas::CanvasDrawingSession^ ds, float x, float y, float Width, float Height) override;

	protected:
		virtual void draw_on_error(Microsoft::Graphics::Canvas::CanvasDrawingSession^ ds, float x, float y, float Width, float Height);

	protected:
		Microsoft::Graphics::Canvas::Text::CanvasTextLayout^ file_svg;
		Microsoft::Graphics::Canvas::Svg::CanvasSvgDocument^ graph_svg;

	protected:
		Windows::Foundation::Size viewport;
	};
}
	