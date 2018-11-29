#pragma once

#include "graphlet/primitive.hpp"

namespace WarGrey::SCADA {
	private enum class ButtonState { Default, Executing, Failed, Ready, Disabled, _ };

	private struct ButtonStyle {
		Microsoft::Graphics::Canvas::Brushes::ICanvasBrush^ border_color;
		Microsoft::Graphics::Canvas::Brushes::ICanvasBrush^ background_color;
		Microsoft::Graphics::Canvas::Brushes::ICanvasBrush^ foreground_color;
		Microsoft::Graphics::Canvas::Text::CanvasTextFormat^ font;
	};

	private class Buttonlet : public WarGrey::SCADA::IStatelet<WarGrey::SCADA::ButtonState, WarGrey::SCADA::ButtonStyle> {
	public:
		Buttonlet(Platform::String^ caption, float width = 128.0F, float height = 32.0F,
			float thickness = 3.0F, float corner_radius = 4.0F,
			Platform::String^ tongue = "menu");

		Buttonlet(ButtonState default_state, Platform::String^ caption, float width = 128.0F, float height = 32.0F,
			float thickness = 2.0F, float corner_radius = 4.0F,
			Platform::String^ tongue = "menu");

	public:
		void fill_extent(float x, float y, float* w = nullptr, float* h = nullptr) override;
		void draw(Microsoft::Graphics::Canvas::CanvasDrawingSession^ ds, float x, float y, float Width, float Height) override;

	public:
		void prepare_style(WarGrey::SCADA::ButtonState status, WarGrey::SCADA::ButtonStyle& style) override;
		void on_state_changed(WarGrey::SCADA::ButtonState status) override;

	private:
		Microsoft::Graphics::Canvas::Text::CanvasTextLayout^ label;

	private:
		Platform::String^ caption;
		float width;
		float height;
		float corner_radius;
		float thickness;

	private:
		bool flashing;
	};
}
