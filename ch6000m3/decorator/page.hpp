#pragma once

#include "decorator/decorator.hpp"

namespace WarGrey::SCADA {
	private class PageDecorator : virtual public WarGrey::SCADA::IPlanetDecorator {
	public:
		PageDecorator(Microsoft::Graphics::Canvas::Brushes::ICanvasBrush ^ brush);

	public:
		void draw_after_snip(WarGrey::SCADA::IGraphlet* snip, Microsoft::Graphics::Canvas::CanvasDrawingSession^ ds,
			float x, float y, float width, float height, bool selected) override;

	protected:
		~PageDecorator() noexcept;

	private:
		Microsoft::Graphics::Canvas::Brushes::ICanvasBrush^ brush;
	};
}
