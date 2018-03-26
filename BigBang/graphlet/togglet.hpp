#pragma once

#include "graphlet/primitive.hpp"

namespace WarGrey::SCADA {
	private class Togglet : public WarGrey::SCADA::IGraphlet {
	public:
		Togglet(bool state0, Platform::String^ checked_label, Platform::String^ unchecked_label, float width = 0.0F,
			Windows::UI::Color& checked_color = Windows::UI::Colors::DodgerBlue,
			Windows::UI::Color& unchecked_color = Windows::UI::Colors::SlateGray);

	public:
		void fill_extent(float x, float y, float* w = nullptr, float* h = nullptr) override;
		void draw(Microsoft::Graphics::Canvas::CanvasDrawingSession^ ds, float x, float y, float Width, float Height) override;
		
	public:
		bool checked();
		void toggle();

	private:
		Microsoft::Graphics::Canvas::Text::CanvasTextLayout^ label;
		Microsoft::Graphics::Canvas::Text::CanvasTextLayout^ unlabel;
		Microsoft::Graphics::Canvas::Brushes::ICanvasBrush^ ckcolor;
		Microsoft::Graphics::Canvas::Brushes::ICanvasBrush^ uncolor;
		Microsoft::Graphics::Canvas::Brushes::ICanvasBrush^ lblcolor;
		Microsoft::Graphics::Canvas::Brushes::ICanvasBrush^ unlblcolor;

	private:
		float width;
		bool state;
	};
}
