#pragma once
#pragma warning(disable: 4250) 

#include "graphlet/msappxlet.hxx"
#include "graphlet/primitive.hpp"

#include "brushes.hxx"

namespace WarGrey::SCADA {
	private class ISvglet abstract : public virtual WarGrey::SCADA::IMsAppxlet<Microsoft::Graphics::Canvas::Svg::CanvasSvgDocument, WarGrey::SCADA::IGraphlet, int> {
	public:
		virtual ~ISvglet() noexcept;
		ISvglet(float width, float height);

	public:
		void construct() override;
		void fill_extent(float x, float y, float* w = nullptr, float* h = nullptr) override;
		void draw(Microsoft::Graphics::Canvas::CanvasDrawingSession^ ds, float x, float y, float Width, float Height) override;
		void draw_progress(Microsoft::Graphics::Canvas::CanvasDrawingSession^ ds, float x, float y, float Width, float Height) override;
		bool ready();

	public:
		virtual Platform::String^ name() = 0;
		virtual Platform::String^ rootdir() = 0;

	protected:
		void on_appx(Windows::Foundation::Uri^ ms_appx_svg, Microsoft::Graphics::Canvas::Svg::CanvasSvgDocument^ doc_svg, int hint) override;
		
	protected:
		virtual void on_ready() {}
		virtual void post_ready() {}

		void set_shape_color(Platform::String^ id, Windows::UI::Color& c);
		void set_shape_color(Platform::String^ id, unsigned int hex, double alpha = 1.0);
		void set_shape_color(Platform::String^ id, WarGrey::SCADA::Colour^ brush);

		Windows::UI::Color get_fill_color(Platform::String^ id, Windows::UI::Color& default_color = Windows::UI::Colors::Transparent);
		void set_fill_color(Platform::String^ id, Windows::UI::Color& c);
		void set_fill_color(Platform::String^ id, unsigned int hex, double alpha = 1.0);
		void set_fill_color(Platform::String^ id, WarGrey::SCADA::Colour^ brush);

		Windows::UI::Color get_stroke_color(Platform::String^ id, Windows::UI::Color& default_color = Windows::UI::Colors::Black);
		void set_stroke_color(Platform::String^ id, Windows::UI::Color& c);
		void set_stroke_color(Platform::String^ id, unsigned int hex, double alpha = 1.0);
		void set_stroke_color(Platform::String^ id, WarGrey::SCADA::Colour^ brush);
		
		float get_stroke_width(Platform::String^ id, float default_width = 1.0F);
		void set_stroke_width(Platform::String^ id, float width);

	protected:
		float get_child_length_attribute(Platform::String^ id, Platform::String^ attribute, bool* relative, float default_value, bool inherited = true);
		float get_child_number_attribute(Platform::String^ id, Platform::String^ attribute, float default_value, bool inherited = true);
		void set_child_percentage_attribute(Platform::String^ id, Platform::String^ attribute, float length);
		void set_child_number_attribute(Platform::String^ id, Platform::String^ attribute, float length);

	private:
		float get_length_attribute(Platform::String^ attribute, bool* relative, float default_value = 100.0F, bool inherited = false);
		void set_percentage_attribute(Platform::String^ attribute, float length);
		void set_number_attribute(Platform::String^ attribute, float length);

		Windows::UI::Color get_child_color_attribute(Platform::String^ id, Platform::String^ attribute, Windows::UI::Color& default_color, bool inherited = true);
		void set_child_color_attribute(Platform::String^ id, Platform::String^ attribute, Windows::UI::Color& c);
		
	protected:
		Windows::Foundation::Size viewport;

	private:
		Microsoft::Graphics::Canvas::Svg::CanvasSvgDocument^ graph_svg;
		Microsoft::Graphics::Canvas::Svg::CanvasSvgNamedElement^ root;
		Windows::Foundation::Uri^ ms_appx_svg;
	};

	private class Svgmaplet : public WarGrey::SCADA::ISvglet {
	public:
		Svgmaplet(Platform::String^ file_svg, float width = 0.0F, float height = 0.0F, Platform::String^ rootdir = "graphlet");
		Svgmaplet(Platform::String^ file_svg, Platform::String^ rootdir);

	public:
		Platform::String^ name() override;
		Platform::String^ rootdir() override;

	private:
		Platform::String^ file_svg;
		Platform::String^ stone_subdir;
	};

	template<typename State, typename Style>
	private class Svglet abstract : public WarGrey::SCADA::ISvglet, public WarGrey::SCADA::IStatelet<State, Style> {
	public:
		Svglet(State status0, float width, float height)
			: WarGrey::SCADA::ISvglet(width, height)
			, WarGrey::SCADA::IStatelet<State, Style>(status0) {}

	public:
		void sprite_construct() override { /* at this point, status maybe not ready for updating */ }

	public:
		Platform::String^ rootdir() override {
			return "graphlet";
		}

	protected:
		void post_ready() override {
			this->update_state();
		}
	};
}
