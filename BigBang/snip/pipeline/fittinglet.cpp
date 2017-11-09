#include "paint.hpp"
#include "shape.hpp"
#include "colorspace.hpp"
#include "snip/pipeline/fittinglet.hpp"
#include "snip/pipeline/static.hpp"

using namespace WarGrey::SCADA;

using namespace Windows::UI;
using namespace Windows::Foundation;
using namespace Windows::Foundation::Numerics;

using namespace Microsoft::Graphics::Canvas;
using namespace Microsoft::Graphics::Canvas::Geometry;

Fittinglet::Fittinglet(float width, float height, float thickness, double color, double saturation, double light, double highlight)
    : width(width), height(height), pipe_thickness(thickness) {
    if (thickness <= 0.0F) {
        this->pipe_thickness = this->height * 0.618F;
    }

    this->pipe_ascent = (this->height - this->pipe_thickness) * 0.5F;

    this->color = hsla(color, saturation, light);
    this->highlight_color = hsla(color, saturation, highlight);
    this->body_color = hsla(color, saturation, light * default_connector_light_rate);
}

void Fittinglet::load() {
    Color colors[] = { this->body_color, this->highlight_color, this->body_color, this->body_color };

    float body_rx = this->pipe_ascent * 0.18F;
    float body_ry = this->height * 0.5F;
    float body_width = this->width - body_rx * 2.0F;

    this->brush = make_linear_gradient_brush(body_rx, 0.0F, body_rx, this->height, MAKE_GRADIENT_STOPS(colors));
    this->body = geometry_freeze(cylinder_rl_surface(body_rx, body_ry, body_width));
}

void Fittinglet::fill_extent(float x, float y, float* w, float* h) {
    SET_VALUES(w, this->width, h, this->height);
}

Rect Fittinglet::get_inlet() {
    float sockect_width = this->brush->StartPoint.x;

    return Rect{ 0.0F, this->pipe_ascent, sockect_width, this->pipe_thickness };
}

Rect Fittinglet::get_outlet() {
    float sockect_width = this->brush->StartPoint.x * 2.0F;

    return Rect{ this->width - sockect_width, this->pipe_ascent, sockect_width, this->pipe_thickness };
}

void Fittinglet::draw(CanvasDrawingSession^ ds, float x, float y, float Width, float Height) {
    float cx = this->brush->StartPoint.x;
    float cy = this->height * 0.5F;
    
    brush_translate(this->brush, x, y);
    ds->FillEllipse(x + this->width - cx, y + cy, cx, cy, this->color);
    ds->DrawCachedGeometry(this->body, x, y, this->brush);
}
