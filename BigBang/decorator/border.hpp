#pragma once

#include "decorator/decorator.hpp"

#include "brushes.hxx"

namespace WarGrey::SCADA {
    private class BorderDecorator : public virtual WarGrey::SCADA::IPlanetDecorator {
    public:
        BorderDecorator(
			Microsoft::Graphics::Canvas::Brushes::CanvasSolidColorBrush^ border_color = WarGrey::SCADA::Colours::AccentDark,
			Microsoft::Graphics::Canvas::Brushes::CanvasSolidColorBrush^ enclosing_color = WarGrey::SCADA::Colours::GrayText);

	public:
		void draw_before(Microsoft::Graphics::Canvas::CanvasDrawingSession^ ds, float Width, float Height) override;

	private:
		Microsoft::Graphics::Canvas::Brushes::CanvasSolidColorBrush^ border_color;
		Microsoft::Graphics::Canvas::Brushes::CanvasSolidColorBrush^ enclosing_color;
    };
}
