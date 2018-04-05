#pragma once

#include "graphlet/primitive.hpp"
#include "brushes.hxx"

namespace WarGrey::SCADA {
	private class Textlet abstract : public virtual WarGrey::SCADA::IGraphlet {
	public:
		void set_text(Platform::String^ content);
		void set_font(Microsoft::Graphics::Canvas::Text::CanvasTextFormat^ font);
		void set_color(Microsoft::Graphics::Canvas::Brushes::CanvasSolidColorBrush^ color = WarGrey::SCADA::Colours::Silver);

	public:
		void set_layout_font_size(int char_idx, int char_count, float size);
		void set_layout_font_style(int char_idx, int char_count, Windows::UI::Text::FontStyle style);

	public:
		void fill_extent(float x, float y, float* w = nullptr, float* h = nullptr) override;
		void draw(Microsoft::Graphics::Canvas::CanvasDrawingSession^ ds, float x, float y, float Width, float Height) override;

	protected:
		virtual void on_font_change() {}

	protected:
		Microsoft::Graphics::Canvas::Text::CanvasTextFormat^ text_font;
		Microsoft::Graphics::Canvas::Brushes::ICanvasBrush^ text_color;
		Microsoft::Graphics::Canvas::Text::CanvasTextLayout^ text_layout;

	private:
		Platform::String^ raw;
	};

    private class Labellet : public virtual WarGrey::SCADA::Textlet {
    public:
        Labellet(const wchar_t* fmt, ...);
		Labellet(Platform::String^ content = "");
	};

	private class Booleanlet : public virtual WarGrey::SCADA::Textlet, public virtual WarGrey::SCADA::IScalelet<bool> {
	public:
		Booleanlet(const wchar_t* fmt, ...);
		Booleanlet(Platform::String^ content = "");

	public:
		void fill_extent(float x, float y, float* w = nullptr, float* h = nullptr) override;
		void draw(Microsoft::Graphics::Canvas::CanvasDrawingSession^ ds, float x, float y, float Width, float Height) override;

	public:
		void on_font_change() override;

	public:
		void set_indicator_color(Microsoft::Graphics::Canvas::Brushes::ICanvasBrush^ true_color,
			Microsoft::Graphics::Canvas::Brushes::ICanvasBrush^ false_color = WarGrey::SCADA::Colours::WhiteSmoke);

	private:
		Microsoft::Graphics::Canvas::Brushes::ICanvasBrush^ true_color;
		Microsoft::Graphics::Canvas::Brushes::ICanvasBrush^ false_color;

	private:
		float indicator_size;
		float gapsize;
	};

	private class ScaleTextlet : public virtual WarGrey::SCADA::Textlet, public virtual WarGrey::SCADA::IScalelet<float> {
	public:
		ScaleTextlet(Platform::String^ unit, Platform::String^ label = "", Platform::String^ subscript = "",
			WarGrey::SCADA::Colour^ label_color = WarGrey::SCADA::Colours::make(0x23EBB9U),
			WarGrey::SCADA::Colour^ scale_color = WarGrey::SCADA::Colours::Yellow);

	public:
		void construct() override;
		void draw(Microsoft::Graphics::Canvas::CanvasDrawingSession^ ds, float x, float y, float Width, float Height) override;
		void fill_extent(float x, float y, float* w = nullptr, float* h = nullptr) override;

	protected:
		void on_scale_change(float scale) override;

	private:
		Microsoft::Graphics::Canvas::Text::CanvasTextLayout^ scale_layout;
		Microsoft::Graphics::Canvas::Text::CanvasTextLayout^ unit_layout;

	private:
		Microsoft::Graphics::Canvas::Brushes::ICanvasBrush^ scale_color;
	};
}
