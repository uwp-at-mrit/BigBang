#pragma once

#include "virtualization/keyboard.hpp"

namespace WarGrey::SCADA {
    private class Numpad : public WarGrey::SCADA::Keyboard {
    public:
		Numpad(WarGrey::SCADA::IPlanet* master, float fontsize = 32.0F);

    public:
		void construct() override;
		void fill_extent(float x, float y, float* w = nullptr, float* h = nullptr) override;
		void fill_auto_position(float* x, float* y, WarGrey::SCADA::IGraphlet* g, WarGrey::SCADA::GraphletAnchor a) override;
		
	public:
		void draw_before(Microsoft::Graphics::Canvas::CanvasDrawingSession^ ds, float x, float y, float Width, float Height) override;
		void draw_cell(Microsoft::Graphics::Canvas::CanvasDrawingSession^ ds,
			Windows::System::VirtualKey key, bool focused, bool tapped,
			float x, float y, float width, float height) override;

    private:
		Microsoft::Graphics::Canvas::Text::CanvasTextFormat^ label_font;
		Microsoft::Graphics::Canvas::Brushes::ICanvasBrush^ foreground;
		Microsoft::Graphics::Canvas::Brushes::ICanvasBrush^ background;
		Microsoft::Graphics::Canvas::Brushes::ICanvasBrush^ border;
		Microsoft::Graphics::Canvas::Brushes::ICanvasBrush^ highlight;
		Microsoft::Graphics::Canvas::Brushes::ICanvasBrush^ taplight;

	private:
		float radius;
	};
}
