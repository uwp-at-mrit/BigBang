#pragma once

#include "virtualization/keyboard.hpp"

namespace WarGrey::SCADA {
    private class Arrowpad : public WarGrey::SCADA::Keyboard {
    public:
		Arrowpad(WarGrey::SCADA::IPlanet* master, float fontsize = 32.0F);

    public:
		void construct() override;

	public:
		void fill_auto_position(float* x, float* y, WarGrey::SCADA::IGraphlet* g, WarGrey::SCADA::GraphletAnchor a) override;

	protected:
		void draw_before(Microsoft::Graphics::Canvas::CanvasDrawingSession^ ds, float x, float y, float Width, float Height) override;
		void draw_cell(Microsoft::Graphics::Canvas::CanvasDrawingSession^ ds,
			Windows::System::VirtualKey key, bool focused, bool tapped,
			float x, float y, float width, float height) override;

	protected:
		Windows::System::VirtualKey find_received_key(unsigned int keycode) override;

    private:
		Microsoft::Graphics::Canvas::Brushes::ICanvasBrush^ foreground;
		Microsoft::Graphics::Canvas::Brushes::ICanvasBrush^ background;
		Microsoft::Graphics::Canvas::Brushes::ICanvasBrush^ border;
		Microsoft::Graphics::Canvas::Brushes::ICanvasBrush^ highlight;
		Microsoft::Graphics::Canvas::Brushes::ICanvasBrush^ taplight;

	private:
		float radius;
	};
}