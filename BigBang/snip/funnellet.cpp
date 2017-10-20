#include "geometry.hpp"
#include "gradient.hpp"
#include "colorspace.hpp"
#include "snip/funnellet.hpp"

using namespace WarGrey::SCADA;

using namespace Windows::UI;
using namespace Microsoft::Graphics::Canvas;

Funnellet::Funnellet(float width, float height, double color, double saturation, double dark, double light) {
    this->width = width;
    this->height = height;

    this->color = hsla(color, saturation, dark);
    this->highlight_color = hsla(color, saturation, light);
}

void Funnellet::load() {
    Color topface_colors[] = { this->color, this->color, this->highlight_color, this->color };
    Color body_colors[] = { this->color, this->highlight_color, this->color };

    auto radiusT = this->width / 2.0F;
    auto radiusB = this->width / 8.0F;
    auto radiusY = this->width / 64.0F;
    auto body_height = this->height - radiusY * 3.0F;

    this->body = geometry_freeze(pyramid_surface(radiusT, radiusB, radiusY, body_height));
    this->body_brush = make_linear_gradient_brush(this->width, 0.0F, MAKE_GRADIENT_STOPS(body_colors));
    
    this->topface = geometry_freeze(ellipse(radiusT, radiusY, radiusT, radiusY));
    this->topface_brush = make_linear_gradient_brush(this->width, 0.0F, MAKE_GRADIENT_STOPS(topface_colors));
}

void Funnellet::fill_extent(float x, float y, float* w, float* h, float* b, float* t, float* l, float* r) {
    SET_VALUES(w, this->width, h, this->height);
    SET_BOXES(b, t, 0.0F);
    SET_BOXES(l, r, 0.0F);
}

void Funnellet::draw(CanvasDrawingSession^ ds, float x, float y, float Width, float Height) {
    brush_translate(this->topface_brush, x, y);
    brush_translate(this->body_brush, x, y);

    // drawing top face after drawing body makes the edge more smoothing.
    ds->DrawCachedGeometry(this->body, x, y, this->body_brush);
    ds->DrawCachedGeometry(this->topface, x, y, this->topface_brush);
}
