#pragma once

#include "decorator/grid.hpp"
#include "system.hpp"
#include "paint.hpp"

using namespace WarGrey::SCADA;

using namespace Windows::Foundation;

using namespace Windows::UI;
using namespace Windows::UI::ViewManagement;

using namespace Microsoft::Graphics::Canvas;
using namespace Microsoft::Graphics::Canvas::Brushes;

GridDecorator::GridDecorator(float w, float h) : width(w), height(h) {
    if (this->height <= 0.0F) {
        this->height = this->width;
    }
}

void GridDecorator::draw_before(IUniverse* master, CanvasDrawingSession^ ds, float Width, float Height) {
    static auto grid_color = make_solid_brush(system_color(UIElementType::GrayText));

    grid_color->Opacity = 0.64F;
    for (float w = this->width; w <= Width; w += this->width) {
        ds->DrawLine(w, 0.0F, w, Height, grid_color);
    }

    for (float h = this->height; h <= Height; h += this->height) {
        ds->DrawLine(0.0F, h, Width, h, grid_color);
    }
}
