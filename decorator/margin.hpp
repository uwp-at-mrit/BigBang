#pragma once

#include "decorator/decorator.hpp"

namespace WarGrey::SCADA {
    private class MarginDecorator : public virtual WarGrey::SCADA::IPlanetDecorator {
    public:
		MarginDecorator(bool only_when_selected = false, bool draw_border = false);

	public:
		void draw_after_graphlet(WarGrey::SCADA::IGraphlet* g, Microsoft::Graphics::Canvas::CanvasDrawingSession^ ds,
			float x, float y, float width, float height, bool selected) override;

	private:
		bool draw_all;
		bool draw_border;
    };
}
