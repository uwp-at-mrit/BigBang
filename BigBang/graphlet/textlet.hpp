#pragma once
#pragma warning (disable: 4250)

#include "graphlet/primitive.hpp"
#include "brushes.hxx"
#include "text.hpp"

namespace WarGrey::SCADA {
	private enum class EditorStatus { Disabled, Enabled, _ };

	private struct DimensionStyle {
		Microsoft::Graphics::Canvas::Text::CanvasTextFormat^ label_font;
		Microsoft::Graphics::Canvas::Brushes::ICanvasBrush^ label_color;
		Microsoft::Graphics::Canvas::Brushes::ICanvasBrush^ label_border_color;
		Microsoft::Graphics::Canvas::Brushes::ICanvasBrush^ label_background_color;

		Microsoft::Graphics::Canvas::Text::CanvasTextFormat^ number_font;
		Microsoft::Graphics::Canvas::Brushes::ICanvasBrush^ number_color;
		Microsoft::Graphics::Canvas::Brushes::ICanvasBrush^ number_border_color;
		Microsoft::Graphics::Canvas::Brushes::ICanvasBrush^ number_background_color;

		Microsoft::Graphics::Canvas::Text::CanvasTextFormat^ unit_font;
		Microsoft::Graphics::Canvas::Brushes::ICanvasBrush^ unit_color;
		Microsoft::Graphics::Canvas::Brushes::ICanvasBrush^ unit_border_color;
		Microsoft::Graphics::Canvas::Brushes::ICanvasBrush^ unit_background_color;

		Microsoft::Graphics::Canvas::Brushes::ICanvasBrush^ caret_color;

		float minimize_number_width = -1.0F;
		float number_xfraction = -1.0F;

		float number_leading_space = -1.0F;
		float number_trailing_space = -1.0F;

		int precision = -1;
	};

	private class ITextlet abstract : public virtual WarGrey::SCADA::IGraphlet {
	public:
		void set_text(const wchar_t* fmt, ...);
		void set_text(Platform::String^ content, WarGrey::SCADA::GraphletAnchor anchor = GraphletAnchor::LT);
		
		void set_text(Platform::String^ content, unsigned int subidx, unsigned int subcount,
			WarGrey::SCADA::GraphletAnchor anchor = GraphletAnchor::LT);
		
		void set_text(Platform::String^ symbol, Platform::String^ subsymbol, Platform::String^ suffix = nullptr,
			WarGrey::SCADA::GraphletAnchor anchor = GraphletAnchor::LT);

	public:
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

	private:
		void set_subtext();

	protected:
		Microsoft::Graphics::Canvas::Text::CanvasTextFormat^ text_font;
		Microsoft::Graphics::Canvas::Brushes::ICanvasBrush^ text_color;
		Microsoft::Graphics::Canvas::Text::CanvasTextLayout^ text_layout;

	private:
		Platform::String^ raw;
		unsigned int sub_index;
		unsigned int sub_count;
		float subscript_fontsize = 0.0F;
	};

    private class Labellet : public virtual WarGrey::SCADA::ITextlet {
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

	private class IEditorlet abstract
		: public virtual WarGrey::SCADA::ITextlet
		, public virtual WarGrey::SCADA::IValuelet<double>
		, public virtual WarGrey::SCADA::IStatuslet<WarGrey::SCADA::EditorStatus, WarGrey::SCADA::DimensionStyle> {
	public:
		IEditorlet(WarGrey::SCADA::EditorStatus default_status, Platform::String^ unit,
			Platform::String^ label, Platform::String^ subscript);

		IEditorlet(WarGrey::SCADA::EditorStatus default_status, WarGrey::SCADA::DimensionStyle& default_style,
			Platform::String^ unit, Platform::String^ label, Platform::String^ subscript);

	public:
		void construct() override;
		void update(long long count, long long interval, long long uptime) override;
		void fill_extent(float x, float y, float* w = nullptr, float* h = nullptr) override;
		void fill_margin(float x, float y, float* t = nullptr, float* r = nullptr, float* b = nullptr, float* l = nullptr) override;
		void draw(Microsoft::Graphics::Canvas::CanvasDrawingSession^ ds, float x, float y, float Width, float Height) override;

	protected:
		void prepare_style(WarGrey::SCADA::EditorStatus status, WarGrey::SCADA::DimensionStyle& style) override;
		void apply_style(WarGrey::SCADA::DimensionStyle& style) override;
		void on_value_changed(double value) override;
		void on_status_changed(WarGrey::SCADA::EditorStatus status) override;

	private:
		Microsoft::Graphics::Canvas::Text::CanvasTextLayout^ number_layout;
		Microsoft::Graphics::Canvas::Text::CanvasTextLayout^ unit_layout;
		WarGrey::SCADA::TextExtent number_box;
		WarGrey::SCADA::TextExtent unit_box;

	private:
		Platform::String^ number;
		Platform::String^ unit;

	private:
		bool flashing;
	};

	private class Dimensionlet : public WarGrey::SCADA::IEditorlet {
	public:
		Dimensionlet(Platform::String^ unit, Platform::String^ label = "", Platform::String^ subscript = "");
		
		Dimensionlet(WarGrey::SCADA::EditorStatus default_status,
			Platform::String^ unit, Platform::String^ label = "", Platform::String^ subscript = "");

		Dimensionlet(WarGrey::SCADA::DimensionStyle& default_style,
			Platform::String^ unit, Platform::String^ label = "", Platform::String^ subscript = "");
		
		Dimensionlet(WarGrey::SCADA::EditorStatus default_status, WarGrey::SCADA::DimensionStyle& default_style,
			Platform::String^ unit, Platform::String^ label = "", Platform::String^ subscript = "");
	};

	private class Percentagelet : public WarGrey::SCADA::IEditorlet {
	public:
		Percentagelet(WarGrey::SCADA::DimensionStyle& style, Platform::String^ label = "", Platform::String^ subscript = "");
		Percentagelet(Platform::String^ label = "", Platform::String^ subscript = "");
	};
}
