#pragma once

#include "snip/snip.hpp"
#include "system.hpp"

namespace WarGrey::SCADA {
	private class Togglet : public WarGrey::SCADA::ISnip {
	public:
		Togglet(bool initial_state, Platform::String^ checked_label, Platform::String^ unchecked_label, float width = 0.0F,
			Windows::UI::Color& checked_color = system_color(Windows::UI::ViewManagement::UIElementType::Highlight),
			Windows::UI::Color& unchecked_color = system_color(Windows::UI::ViewManagement::UIElementType::InactiveCaption),
			Windows::UI::Color& label_color = system_color(Windows::UI::ViewManagement::UIElementType::HighlightText));

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

	private:
		float width;
		bool state;
	};
}
