#pragma once

#include "graphlet/primitive.hpp"
#include "brushes.hxx"

namespace WarGrey::SCADA {
	private struct ToggleStyle {
		Microsoft::Graphics::Canvas::Text::CanvasTextFormat^ font;

		WarGrey::SCADA::Colour^ checked_color;
		WarGrey::SCADA::Colour^ unchecked_color;

		Microsoft::Graphics::Canvas::Brushes::ICanvasBrush^ checked_label_color;
		Microsoft::Graphics::Canvas::Brushes::ICanvasBrush^ unchecked_label_color;
	};

	WarGrey::SCADA::ToggleStyle make_toggle_style(float fontsize = 0.0F, WarGrey::SCADA::Colour^ checked_color = nullptr, WarGrey::SCADA::Colour^ unchecked_color = nullptr);

	private class Togglet : public virtual WarGrey::SCADA::IGraphlet {
	public:
		Togglet(bool state0, Platform::String^ label, float width = 0.0F, Platform::String^ tongue = "menu");
		Togglet(WarGrey::SCADA::ToggleStyle& style, bool state0, Platform::String^ label, float width = 0.0F, Platform::String^ tongue = "menu");

		Togglet(bool state0, Platform::String^ checked_label, Platform::String^ unchecked_label, float width = 0.0F, Platform::String^ tongue = "menu");
		Togglet(WarGrey::SCADA::ToggleStyle& style, bool state0, Platform::String^ checked_label, Platform::String^ unchecked_label, float width = 0.0F, Platform::String^ tongue = "menu");

	public:
		void fill_extent(float x, float y, float* w = nullptr, float* h = nullptr) override;
		void draw(Microsoft::Graphics::Canvas::CanvasDrawingSession^ ds, float x, float y, float Width, float Height) override;
		
	public:
		bool checked();
		void toggle();

	private:
		void prepare_style();

	private:
		Microsoft::Graphics::Canvas::Text::CanvasTextLayout^ label;
		Microsoft::Graphics::Canvas::Text::CanvasTextLayout^ unlabel;

	private:
		WarGrey::SCADA::ToggleStyle style;

	private:
		float width;
		bool state;
	};
}
