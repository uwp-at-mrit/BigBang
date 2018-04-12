#pragma once

#include "graphlet/primitive.hpp"
#include "credit.hpp"
#include "turtle.hpp"
#include "brushes.hxx"

namespace WarGrey::SCADA {
	private class Svglet : public virtual WarGrey::SCADA::IGraphlet {
	public:
		~Svglet() noexcept;

		Svglet(Platform::String^ file_svg, float width, float height, Platform::String^ rootdir = "graphlet");
		Svglet(Platform::String^ file_svg, Platform::String^ rootdir = "graphlet");

	public:
		void construct() override;
		void fill_extent(float x, float y, float* w = nullptr, float* h = nullptr) override;
		void draw(Microsoft::Graphics::Canvas::CanvasDrawingSession^ ds, float x, float y, float Width, float Height) override;

	protected:
		virtual void draw_on_error(Microsoft::Graphics::Canvas::CanvasDrawingSession^ ds, float x, float y, float Width, float Height);

	private:
		float get_length_attribute(Platform::String^ attribute_name,
			Microsoft::Graphics::Canvas::Svg::CanvasSvgLengthUnits* units,
			bool inherited = false);

	protected:
		Microsoft::Graphics::Canvas::Svg::CanvasSvgDocument^ graph_svg;
		Microsoft::Graphics::Canvas::Svg::CanvasSvgNamedElement^ root;

	protected:
		Windows::Foundation::Size viewport;
		Windows::Foundation::Uri^ ms_appx_svg;
	};
}
	