#pragma once

#include "decorator/decorator.hpp"

namespace WarGrey::SCADA {
    private class BorderDecorator : public virtual WarGrey::SCADA::IPlanetDecorator {
    public:
        BorderDecorator(bool draw_border = true, bool draw_enclosing = false);

	public:
		void draw_before(
			WarGrey::SCADA::IPlanet* master,
			Microsoft::Graphics::Canvas::CanvasDrawingSession^ ds,
			float Width, float Height) override;

	private:
		bool draw_border;
		bool draw_enclosing_box;
    };
}
