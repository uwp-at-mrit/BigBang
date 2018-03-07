#pragma once

#include "decorator/decorator.hpp"

namespace WarGrey::SCADA {
    private class GridDecorator : public virtual WarGrey::SCADA::IPlanetDecorator {
    public:
        GridDecorator(float grid_width = 16.0F, float grid_height = 0.0F);

    public:
        void draw_before(
            WarGrey::SCADA::IPlanet* master,
            Microsoft::Graphics::Canvas::CanvasDrawingSession^ ds,
            float Width, float Height) override;

	protected:
		~GridDecorator() noexcept {}

    private:
        float width;
        float height;
    };
}
