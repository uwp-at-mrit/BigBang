#pragma once

#include "graphlet/primitive.hpp"
#include "brushes.hxx"
#include "text.hpp"

namespace WarGrey::SCADA {
	private class Textlet abstract : public virtual WarGrey::SCADA::IGraphlet {
	public:
		void set_text(const wchar_t* fmt, ...);
		void set_text(Platform::String^ content, WarGrey::SCADA::GraphletAnchor anchor = GraphletAnchor::LT);
		void set_font(Microsoft::Graphics::Canvas::Text::CanvasTextFormat^ font, WarGrey::SCADA::GraphletAnchor anchor = GraphletAnchor::LT);
		void set_color(unsigned int color_hex, double alpha = 1.0);
		void set_color(Microsoft::Graphics::Canvas::Brushes::ICanvasBrush^ color = WarGrey::SCADA::Colours::Silver);

	public:
		void fill_extent(float x, float y, float* w = nullptr, float* h = nullptr) override;
		void fill_margin(float x, float y, float* t = nullptr, float* r = nullptr, float* b = nullptr, float* l = nullptr) override;
		void draw(Microsoft::Graphics::Canvas::CanvasDrawingSession^ ds, float x, float y, float Width, float Height) override;

	protected:
		virtual void on_font_changed() {}

	protected:
		void set_layout_font_size(int char_idx, int char_count);
		void set_layout_font_size(int char_idx, int char_count, float size);
		void set_layout_font_style(int char_idx, int char_count, Windows::UI::Text::FontStyle style);
	
	protected:
		void set_subtext(Platform::String^ symbol, Platform::String^ subsymbol, Platform::String^ suffix = nullptr);

	protected:
		Microsoft::Graphics::Canvas::Text::CanvasTextFormat^ text_font;
		Microsoft::Graphics::Canvas::Brushes::ICanvasBrush^ text_color;
		Microsoft::Graphics::Canvas::Text::CanvasTextLayout^ text_layout;

	private:
		Platform::String^ raw;
		float subscript_fontsize = 0.0F;
	};

    private class Labellet : public virtual WarGrey::SCADA::Textlet {
    public:
        Labellet(const wchar_t* fmt, ...);

		Labellet(Platform::String^ caption = "",
			Microsoft::Graphics::Canvas::Text::CanvasTextFormat^ font = nullptr,
			Microsoft::Graphics::Canvas::Brushes::ICanvasBrush^ color = nullptr);

		Labellet(Platform::String^ caption, Platform::String^ subscript,
			Microsoft::Graphics::Canvas::Text::CanvasTextFormat^ font = nullptr,
			Microsoft::Graphics::Canvas::Brushes::ICanvasBrush^ color = nullptr);

		Labellet(Platform::String^ caption,
			Microsoft::Graphics::Canvas::Text::CanvasTextFormat^ font,
			unsigned int color_hex, double alpha = 1.0);

		Labellet(Platform::String^ caption, Platform::String^ subscript,
			Microsoft::Graphics::Canvas::Text::CanvasTextFormat^ font,
			unsigned int color_hex, double alpha = 1.0);

		Labellet(Platform::String^ caption, Microsoft::Graphics::Canvas::Brushes::ICanvasBrush^ color);
		Labellet(Platform::String^ caption, Platform::String^ subscript, Microsoft::Graphics::Canvas::Brushes::ICanvasBrush^ color);
		Labellet(Platform::String^ caption, unsigned int color_hex, double alpha = 1.0);
		Labellet(Platform::String^ caption, Platform::String^ subscript, unsigned int color_hex, double alpha = 1.0);
	};

	private class Dimensionlet : public virtual WarGrey::SCADA::Textlet, public virtual WarGrey::SCADA::IValuelet<float> {
	public:
		Dimensionlet(Platform::String^ unit, Platform::String^ label = "", Platform::String^ subscript = "",
			Microsoft::Graphics::Canvas::Text::CanvasTextFormat^ num_font = nullptr,
			Microsoft::Graphics::Canvas::Text::CanvasTextFormat^ label_font = nullptr,
			Microsoft::Graphics::Canvas::Brushes::ICanvasBrush^ num_color = nullptr,
			Microsoft::Graphics::Canvas::Brushes::ICanvasBrush^ label_color = nullptr);

		Dimensionlet(Platform::String^ unit,
			Microsoft::Graphics::Canvas::Brushes::ICanvasBrush^ num_color,
			Microsoft::Graphics::Canvas::Brushes::ICanvasBrush^ label_color);

		Dimensionlet(Platform::String^ unit, Platform::String^ label,
			Microsoft::Graphics::Canvas::Brushes::ICanvasBrush^ num_color,
			Microsoft::Graphics::Canvas::Brushes::ICanvasBrush^ label_color);

		Dimensionlet(Platform::String^ unit,
			Microsoft::Graphics::Canvas::Text::CanvasTextFormat^ font,
			Microsoft::Graphics::Canvas::Brushes::ICanvasBrush^ num_color,
			Microsoft::Graphics::Canvas::Brushes::ICanvasBrush^ label_color);

		Dimensionlet(Platform::String^ unit, Platform::String^ label,
			Microsoft::Graphics::Canvas::Text::CanvasTextFormat^ font,
			Microsoft::Graphics::Canvas::Brushes::ICanvasBrush^ num_color,
			Microsoft::Graphics::Canvas::Brushes::ICanvasBrush^ label_color);

		Dimensionlet(Platform::String^ unit,
			Microsoft::Graphics::Canvas::Text::CanvasTextFormat^ num_font,
			Microsoft::Graphics::Canvas::Text::CanvasTextFormat^ unit_font,
			Microsoft::Graphics::Canvas::Brushes::ICanvasBrush^ color);

		Dimensionlet(Platform::String^ unit, Platform::String^ label,
			Microsoft::Graphics::Canvas::Text::CanvasTextFormat^ num_font,
			Microsoft::Graphics::Canvas::Text::CanvasTextFormat^ unit_font,
			Microsoft::Graphics::Canvas::Brushes::ICanvasBrush^ color);

		Dimensionlet(Platform::String^ unit,
			Microsoft::Graphics::Canvas::Text::CanvasTextFormat^ font,
			Microsoft::Graphics::Canvas::Brushes::ICanvasBrush^ color);

		Dimensionlet(Platform::String^ unit, Platform::String^ label,
			Microsoft::Graphics::Canvas::Text::CanvasTextFormat^ font,
			Microsoft::Graphics::Canvas::Brushes::ICanvasBrush^ color);

	public:
		void construct() override;
		void fill_extent(float x, float y, float* w = nullptr, float* h = nullptr) override;
		void fill_margin(float x, float y, float* t = nullptr, float* r = nullptr, float* b = nullptr, float* l = nullptr) override;
		void draw(Microsoft::Graphics::Canvas::CanvasDrawingSession^ ds, float x, float y, float Width, float Height) override;

	protected:
		void on_value_changed(float value) override;

	private:
		Microsoft::Graphics::Canvas::Text::CanvasTextFormat^ num_font;
		Microsoft::Graphics::Canvas::Text::CanvasTextLayout^ num_layout;
		Microsoft::Graphics::Canvas::Text::CanvasTextLayout^ unit_layout;

	private:
		Microsoft::Graphics::Canvas::Brushes::ICanvasBrush^ num_color;

	private:
		WarGrey::SCADA::TextExtent num_box;
		WarGrey::SCADA::TextExtent unit_box;
	};

	private class Percentagelet : public virtual WarGrey::SCADA::Textlet, public virtual WarGrey::SCADA::IValuelet<double> {
	public:
		Percentagelet(Platform::String^ label = "", Platform::String^ subscript = "",
			Microsoft::Graphics::Canvas::Text::CanvasTextFormat^ num_font = nullptr,
			Microsoft::Graphics::Canvas::Text::CanvasTextFormat^ label_font = nullptr,
			Microsoft::Graphics::Canvas::Brushes::ICanvasBrush^ num_color = nullptr,
			Microsoft::Graphics::Canvas::Brushes::ICanvasBrush^ label_color = nullptr);

		Percentagelet(Microsoft::Graphics::Canvas::Text::CanvasTextFormat^ font,
			Microsoft::Graphics::Canvas::Brushes::ICanvasBrush^ num_color,
			Microsoft::Graphics::Canvas::Brushes::ICanvasBrush^ label_color);

		Percentagelet(Platform::String^ label,
			Microsoft::Graphics::Canvas::Text::CanvasTextFormat^ font,
			Microsoft::Graphics::Canvas::Brushes::ICanvasBrush^ num_color,
			Microsoft::Graphics::Canvas::Brushes::ICanvasBrush^ label_color);

		Percentagelet(Microsoft::Graphics::Canvas::Brushes::ICanvasBrush^ num_color,
			Microsoft::Graphics::Canvas::Brushes::ICanvasBrush^ label_color);

		Percentagelet(Platform::String^ label,
			Microsoft::Graphics::Canvas::Brushes::ICanvasBrush^ num_color,
			Microsoft::Graphics::Canvas::Brushes::ICanvasBrush^ label_color);

		Percentagelet(Microsoft::Graphics::Canvas::Text::CanvasTextFormat^ num_font,
			Microsoft::Graphics::Canvas::Text::CanvasTextFormat^ unit_font,
			Microsoft::Graphics::Canvas::Brushes::ICanvasBrush^ color);

		Percentagelet(Platform::String^ label,
			Microsoft::Graphics::Canvas::Text::CanvasTextFormat^ num_font,
			Microsoft::Graphics::Canvas::Text::CanvasTextFormat^ unit_font,
			Microsoft::Graphics::Canvas::Brushes::ICanvasBrush^ color);

		Percentagelet(Microsoft::Graphics::Canvas::Text::CanvasTextFormat^ font,
			Microsoft::Graphics::Canvas::Brushes::ICanvasBrush^ color);

		Percentagelet(Platform::String^ label,
			Microsoft::Graphics::Canvas::Text::CanvasTextFormat^ font,
			Microsoft::Graphics::Canvas::Brushes::ICanvasBrush^ color);

	public:
		void construct() override;
		void fill_extent(float x, float y, float* w = nullptr, float* h = nullptr) override;
		void fill_margin(float x, float y, float* t = nullptr, float* r = nullptr, float* b = nullptr, float* l = nullptr) override;
		void draw(Microsoft::Graphics::Canvas::CanvasDrawingSession^ ds, float x, float y, float Width, float Height) override;

	protected:
		void on_value_changed(double value) override;

	private:
		Microsoft::Graphics::Canvas::Text::CanvasTextFormat^ num_font;
		Microsoft::Graphics::Canvas::Text::CanvasTextLayout^ num_layout;
		Microsoft::Graphics::Canvas::Text::CanvasTextLayout^ sign_layout;

	private:
		Microsoft::Graphics::Canvas::Brushes::ICanvasBrush^ num_color;

	private:
		WarGrey::SCADA::TextExtent num_box;
		WarGrey::SCADA::TextExtent sign_box;
	};
}
