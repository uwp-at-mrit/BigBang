#pragma once

#include "graphlet/primitive.hpp"
#include "brushes.hxx"

namespace WarGrey::SCADA {
	private class Togglet : public virtual WarGrey::SCADA::IGraphlet {
	public:
		Togglet(bool state0, Platform::String^ checked_label, Platform::String^ unchecked_label, float width = 0.0F,
			WarGrey::SCADA::Colour^ checked_color = nullptr, WarGrey::SCADA::Colour^ unchecked_color = nullptr,
			Platform::String^ tongue = "menu");

	public:
		void fill_extent(float x, float y, float* w = nullptr, float* h = nullptr) override;
		void draw(Microsoft::Graphics::Canvas::CanvasDrawingSession^ ds, float x, float y, float Width, float Height) override;
		
	public:
		bool checked();
		void toggle();

	private:
		Microsoft::Graphics::Canvas::Text::CanvasTextLayout^ label;
		Microsoft::Graphics::Canvas::Text::CanvasTextLayout^ unlabel;
		Microsoft::Graphics::Canvas::Brushes::CanvasSolidColorBrush^ ckcolor;
		Microsoft::Graphics::Canvas::Brushes::CanvasSolidColorBrush^ uncolor;
		Microsoft::Graphics::Canvas::Brushes::ICanvasBrush^ lblcolor;
		Microsoft::Graphics::Canvas::Brushes::ICanvasBrush^ unlblcolor;

	private:
		float width;
		bool state;
	};
}
