#pragma once

#include "graphlet/primitive.hpp"

namespace WarGrey::SCADA {
	private enum class ButtonStatus { Default, Executing, Failed, Ready, Disabled, _ };

	private struct ButtonStyle {
		Microsoft::Graphics::Canvas::Brushes::ICanvasBrush^ border_color;
		Microsoft::Graphics::Canvas::Brushes::ICanvasBrush^ background_color;
		Microsoft::Graphics::Canvas::Brushes::ICanvasBrush^ foreground_color;
		Microsoft::Graphics::Canvas::Text::CanvasTextFormat^ font;
	};

	private class Buttonlet : public WarGrey::SCADA::IStatuslet<WarGrey::SCADA::ButtonStatus, WarGrey::SCADA::ButtonStyle> {
	public:
		Buttonlet(Platform::String^ caption, float width = 100.0F, float height = 22.0F, float thickness = 2.0F);
		Buttonlet(ButtonStatus default_status, Platform::String^ caption, float width = 100.0F, float height = 22.0F, float thickness = 2.0F);

	public:
		void fill_extent(float x, float y, float* w = nullptr, float* h = nullptr) override;
		void draw(Microsoft::Graphics::Canvas::CanvasDrawingSession^ ds, float x, float y, float Width, float Height) override;

	public:
		void prepare_style(WarGrey::SCADA::ButtonStatus status, WarGrey::SCADA::ButtonStyle& style) override;
		void on_status_changed(WarGrey::SCADA::ButtonStatus status) override;

	private:
		Microsoft::Graphics::Canvas::Text::CanvasTextLayout^ label;

	private:
		Platform::String^ caption;
		float width;
		float height;
		float thickness;

	private:
		bool flashing;
	};
}
