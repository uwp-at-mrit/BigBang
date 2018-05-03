#pragma once

#include "graphlet/msappxlet.hxx"

#include "brushes.hxx"

namespace WarGrey::SCADA {
	private class Svglet : public virtual WarGrey::SCADA::IMsAppxlet<Microsoft::Graphics::Canvas::Svg::CanvasSvgDocument> {
	public:
		virtual ~Svglet() noexcept;

		Svglet(Platform::String^ file_svg, float width = 0.0F, float height = 0.0F, Platform::String^ rootdir = "graphlet");
		Svglet(Platform::String^ file_svg, Platform::String^ rootdir);

	public:
		void construct() override;
		void fill_extent(float x, float y, float* w = nullptr, float* h = nullptr) override;
		void draw(Microsoft::Graphics::Canvas::CanvasDrawingSession^ ds, float x, float y, float Width, float Height) override;
		void draw_progress(Microsoft::Graphics::Canvas::CanvasDrawingSession^ ds, float x, float y, float Width, float Height) override;
		bool ready();

	protected:
		void on_appx(Windows::Foundation::Uri^ ms_appx_svg, Microsoft::Graphics::Canvas::Svg::CanvasSvgDocument^ doc_svg) override;
		
	protected:
		Windows::UI::Color get_fill_color(Platform::String^ id, Windows::UI::Color& default_color = Windows::UI::Colors::Transparent);
		void set_fill_color(Platform::String^ id, Windows::UI::Color& c);
		void set_fill_color(Platform::String^ id, unsigned int hex, double alpha = 1.0);
		void set_fill_color(Platform::String^ id, WarGrey::SCADA::Colour^ brush);

		Windows::UI::Color get_stroke_color(Platform::String^ id, Windows::UI::Color& default_color = Windows::UI::Colors::Black);
		void set_stroke_color(Platform::String^ id, Windows::UI::Color& c);
		void set_stroke_color(Platform::String^ id, unsigned int hex, double alpha = 1.0);
		void set_stroke_color(Platform::String^ id, WarGrey::SCADA::Colour^ brush);

	private:
		Windows::UI::Color get_child_color_attribute(Platform::String^ id, Platform::String^ attribute_name, Windows::UI::Color& default_color);
		void set_child_color_attribute(Platform::String^ id, Platform::String^ attribute_name, Windows::UI::Color& c);

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
	