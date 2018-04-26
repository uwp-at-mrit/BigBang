#pragma once

#include "decorator/decorator.hpp"

namespace WarGrey::SCADA {
	private class BackgroundDecorator : virtual public WarGrey::SCADA::IPlanetDecorator {
	public:
		BackgroundDecorator(float inset = 0.0F);
		BackgroundDecorator(float horizontal_inset, float vertical_inset);
		BackgroundDecorator(float top_inset, float right_inset, float bottom_inset, float left_inset);

	public:
		void draw_before(WarGrey::SCADA::IPlanet* master,
			Microsoft::Graphics::Canvas::CanvasDrawingSession^ ds,
			float Width, float Height) override;

	protected:
		~BackgroundDecorator() noexcept;

	private:
		Microsoft::Graphics::Canvas::Brushes::ICanvasBrush^ brush;
		float top_inset;
		float right_inset;
		float bottom_inset;
		float left_inset;
	};
}
