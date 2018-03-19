#pragma once

#include "snip/iconlet.hpp"

namespace WarGrey::SCADA {
	private class Snowflakelet : public WarGrey::SCADA::Iconlet {
	public:
		Snowflakelet(float size, Windows::UI::Color& color = Windows::UI::Colors::SteelBlue);

	public:
		void construct() override;
		void draw(Microsoft::Graphics::Canvas::CanvasDrawingSession^ ds, float x, float y, float Width, float Height) override;

	private:
		Microsoft::Graphics::Canvas::Geometry::CanvasCachedGeometry^ snowflake;
		Microsoft::Graphics::Canvas::Brushes::ICanvasBrush^ color;
	};
}
