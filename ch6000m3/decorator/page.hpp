#pragma once

#include "decorator/decorator.hpp"

#include "brushes.hxx"

namespace WarGrey::SCADA {
	private class PageDecorator : virtual public WarGrey::SCADA::IPlanetDecorator {
	public:
		PageDecorator(Microsoft::Graphics::Canvas::Brushes::ICanvasBrush ^ brush = WarGrey::SCADA::Colours::Silver);

	public:
		void draw_after_graphlet(WarGrey::SCADA::IGraphlet* g, Microsoft::Graphics::Canvas::CanvasDrawingSession^ ds,
			float x, float y, float width, float height, bool selected) override;

	protected:
		~PageDecorator() noexcept;

	private:
		Microsoft::Graphics::Canvas::Brushes::ICanvasBrush^ brush;
	};
}
