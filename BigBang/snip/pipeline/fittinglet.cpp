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

Fittinglet::Fittinglet(float width, float height, float hsocket, double color, double saturation, double light, double highlight)
    : width(width), height(height), socket_height(hsocket) {
    if (hsocket <= 0.0F) {
        this->socket_height = this->height * 0.618F;
    }

    this->color = hsla(color, saturation, light);
    this->highlight_color = hsla(color, saturation, highlight);
    this->body_color = hsla(color, saturation, light * default_connector_light_rate);
}

void Fittinglet::load() {
    Color colors[] = { this->body_color, this->highlight_color, this->body_color, this->body_color };

    float body_ry = this->height * 0.5F;
    float body_rx = body_ry * 0.0618F;
    float body_width = this->width - body_rx * 2.0F;

    this->brush = make_linear_gradient_brush(body_rx, 0.0F, body_rx, this->height, MAKE_GRADIENT_STOPS(colors));
    this->body = this->make_body(body_rx, body_ry, body_width);
}

void Fittinglet::fill_extent(float x, float y, float* w, float* h) {
    SET_VALUES(w, this->width, h, this->height);
}

/*************************************************************************************************/
LFittinglet::LFittinglet(float width, float height, float hsocket, double color, double saturation, double light, double highlight)
    : Fittinglet(width, height, socket_height, color, saturation, light, highlight) {}

CanvasCachedGeometry^ LFittinglet::make_body(float rx, float ry, float width) {
    return geometry_freeze(cylinder_rl_surface(rx, ry, width));
}

Rect LFittinglet::get_inlet() {
    float socket_width = this->brush->StartPoint.x * 2.0F;
    float ascent = (this->height - this->socket_height) * 0.5F;

    return Rect{ 0.0F, ascent, socket_width, this->socket_height };
}

Rect LFittinglet::get_outlet() {
    float socket_width = this->brush->StartPoint.x;
    float ascent = (this->height - this->socket_height) * 0.5F;

    return Rect{ this->width - socket_width, ascent, socket_width, this->socket_height };
}

void LFittinglet::draw(CanvasDrawingSession^ ds, float x, float y, float Width, float Height) {
    float rx = this->brush->StartPoint.x;
    float ry = this->height * 0.5F;
    float cx = x + this->width - rx * 1.1618F;
    float cy = y + ry;
    
    brush_translate(this->brush, x, y);
    ds->FillEllipse(cx, cy, rx, ry, this->color);
    ds->DrawCachedGeometry(this->body, x, y, this->brush);
}

/*************************************************************************************************/
RFittinglet::RFittinglet(float width, float height, float hsocket, double color, double saturation, double light, double highlight)
    : Fittinglet(width, height, socket_height, color, saturation, light, highlight) {}

CanvasCachedGeometry^ RFittinglet::make_body(float rx, float ry, float width) {
    return geometry_freeze(cylinder_lr_surface(rx, ry, width));
}

Rect RFittinglet::get_inlet() {
    float socket_width = this->brush->StartPoint.x * 2.0F;
    float ascent = (this->height - this->socket_height) * 0.5F;

    return Rect{ 0.0F, ascent, socket_width, this->socket_height };
}

Rect RFittinglet::get_outlet() {
    float socket_width = this->brush->StartPoint.x;
    float ascent = (this->height - this->socket_height) * 0.5F;

    return Rect{ this->width - socket_width, ascent, socket_width, this->socket_height };
}

void RFittinglet::draw(CanvasDrawingSession^ ds, float x, float y, float Width, float Height) {
    float rx = this->brush->StartPoint.x;
    float ry = this->height * 0.5F;
    float cx = x + rx * 1.1618F;
    float cy = y + ry;

    brush_translate(this->brush, x, y);
    ds->FillEllipse(cx, cy, rx, ry, this->color);
    ds->DrawCachedGeometry(this->body, x, y, this->brush);
}
