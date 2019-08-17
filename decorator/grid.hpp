#pragma once

#include "decorator/decorator.hpp"

namespace WarGrey::SCADA {
    private class GridDecorator : public virtual WarGrey::SCADA::IPlanetDecorator {
    public:
        GridDecorator(float grid_width = 16.0F, float grid_height = 0.0F, float start_x = 0.0F, float start_y = 0.0F);

    public:
        void draw_before(Microsoft::Graphics::Canvas::CanvasDrawingSession^ ds, float X, float Y, float Width, float Height) override;

	public:
		float get_grid_width();
		void set_grid_width(float new_width, float start_x = 0.0F);

		float get_grid_height();
		void set_grid_height(float new_height, float start_y = 0.0F);

    private:
        float gwidth;
        float gheight;
		float x0;
		float y0;
    };
}
