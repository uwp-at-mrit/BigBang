#pragma once

#include "snip.hpp"

namespace WarGrey::SCADA {
	private class Textlet : public WarGrey::SCADA::Snip {};

    private class Labellet : public WarGrey::SCADA::Textlet {
    public:
        Labellet(const wchar_t* fmt, ...);
        Labellet(Platform::String^ content = "");

    public:
        void change_text(Platform::String^ content);

    public:
        void draw(Microsoft::Graphics::Canvas::CanvasDrawingSession^ ds, float x, float y, float Width, float Height) override;
        void fill_extent(float x, float y, float* w = nullptr, float* h = nullptr) override;

    private:
        Platform::String^ content;
        Microsoft::Graphics::Canvas::Text::CanvasTextFormat^ label_font;
    };

	private class Scalelet : public WarGrey::SCADA::Textlet {
	public:
		Scalelet(Platform::String^ label, Platform::String^ unit, Platform::String^ subscript = nullptr,
			Windows::UI::Color& label_color = Windows::UI::ColorHelper::FromArgb(255, 35, 235, 185),
			Windows::UI::Color& scale_color = Windows::UI::Colors::Yellow);

	public:
		void change_scale(float value);

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
