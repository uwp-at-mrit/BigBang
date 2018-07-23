#pragma once

#include "brushes.hxx"
#include "decorator/decorator.hpp"

namespace WarGrey::SCADA {
	private class BackgroundDecorator : virtual public WarGrey::SCADA::IPlanetDecorator {
	public:
		BackgroundDecorator(unsigned int color, float inset = 0.0F);
		BackgroundDecorator(unsigned int color, float horizontal_inset, float vertical_inset);
		BackgroundDecorator(unsigned int color, float top_inset, float right_inset, float bottom_inset, float left_inset);

		BackgroundDecorator(Microsoft::Graphics::Canvas::Brushes::ICanvasBrush^ color, float inset = 0.0F);
		BackgroundDecorator(Microsoft::Graphics::Canvas::Brushes::ICanvasBrush^ color, float horizontal_inset, float vertical_inset);
		BackgroundDecorator(Microsoft::Graphics::Canvas::Brushes::ICanvasBrush^ color, float top_inset, float right_inset, float bottom_inset, float left_inset);

	public:
		void draw_before(Microsoft::Graphics::Canvas::CanvasDrawingSession^ ds, float Width, float Height) override;

	protected:
		~BackgroundDecorator() noexcept;

	private:
		Microsoft::Graphics::Canvas::Brushes::ICanvasBrush^ color;
		float top_inset;
		float right_inset;
		float bottom_inset;
		float left_inset;
	};
}
