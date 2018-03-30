#pragma once

#include "graphlet/primitive.hpp"
#include "brushes.hxx"

namespace WarGrey::SCADA {
	private class Textlet : public WarGrey::SCADA::IGraphlet {};

    private class Labellet : public WarGrey::SCADA::Textlet {
    public:
        Labellet(const wchar_t* fmt, ...);
		Labellet(WarGrey::SCADA::Colour^ color, const wchar_t* fmt, ...);
		Labellet(Microsoft::Graphics::Canvas::Text::CanvasTextFormat^ font, const wchar_t* fmt, ...);
		Labellet(Microsoft::Graphics::Canvas::Text::CanvasTextFormat^ font, WarGrey::SCADA::Colour^ color, const wchar_t* fmt, ...);

		Labellet(Platform::String^ content = "");
		Labellet(WarGrey::SCADA::Colour^ color, Platform::String^ content = "");
		Labellet(Microsoft::Graphics::Canvas::Text::CanvasTextFormat^ font, Platform::String^ content = "");
		Labellet(Microsoft::Graphics::Canvas::Text::CanvasTextFormat^ font, WarGrey::SCADA::Colour^ color, Platform::String^ content = "");

    public:
        void set_text(Platform::String^ content);
		void set_font(Microsoft::Graphics::Canvas::Text::CanvasTextFormat^ font);
		void set_color(Microsoft::Graphics::Canvas::Brushes::CanvasSolidColorBrush^ color = WarGrey::SCADA::Colours::Silver);

    public:
        void draw(Microsoft::Graphics::Canvas::CanvasDrawingSession^ ds, float x, float y, float Width, float Height) override;
        void fill_extent(float x, float y, float* w = nullptr, float* h = nullptr) override;

    private:
        Microsoft::Graphics::Canvas::Text::CanvasTextLayout^ content;
        Microsoft::Graphics::Canvas::Text::CanvasTextFormat^ label_font;
		Microsoft::Graphics::Canvas::Brushes::CanvasSolidColorBrush^ label_color;
    };

	private class Scalelet : public WarGrey::SCADA::Textlet {
	public:
		Scalelet(Platform::String^ unit, Platform::String^ label = "", Platform::String^ subscript = "",
			WarGrey::SCADA::Colour^ label_color = WarGrey::SCADA::Colours::make(0x23EBB9U),
			WarGrey::SCADA::Colour^ scale_color = WarGrey::SCADA::Colours::Yellow);

	public:
		void set_scale(float value);

	public:
		void draw(Microsoft::Graphics::Canvas::CanvasDrawingSession^ ds, float x, float y, float Width, float Height) override;
		void fill_extent(float x, float y, float* w = nullptr, float* h = nullptr) override;

	private:
		Microsoft::Graphics::Canvas::Text::CanvasTextLayout^ label;
		Microsoft::Graphics::Canvas::Text::CanvasTextLayout^ scale;
		Microsoft::Graphics::Canvas::Text::CanvasTextLayout^ unit;

	private:
		Microsoft::Graphics::Canvas::Text::CanvasTextFormat^ scale_font;
		Microsoft::Graphics::Canvas::Brushes::ICanvasBrush^ label_color;
		Microsoft::Graphics::Canvas::Brushes::ICanvasBrush^ scale_color;
	};
}
