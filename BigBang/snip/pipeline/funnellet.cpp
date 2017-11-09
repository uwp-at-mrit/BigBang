#include <collection.h>

#include "paint.hpp"
#include "shape.hpp"
#include "colorspace.hpp"
#include "snip/pipeline/funnellet.hpp"

using namespace WarGrey::SCADA;

using namespace Windows::UI;
using namespace Windows::Foundation;
using namespace Platform::Collections;

using namespace Microsoft::Graphics::Canvas;
using namespace Microsoft::Graphics::Canvas::Geometry;

static const float default_ratio = 1.8F;
static const Color particle_color = Colors::Sienna;

Funnellet::Funnellet(float width, float height, double color, double saturation, double dark, double light) {
    this->width = width;
    this->height = height;
    
    if (height == 0.0F) {
        this->height = width * default_ratio;
    } else if (height < 0.0F) {
        this->height = -(width * height);
    }

    this->color = hsla(color, saturation, dark);
    this->highlight_color = hsla(color, saturation, light);
}

void Funnellet::load() {
    Color topface_colors[] = { this->color, this->color, this->highlight_color, this->color };
    Color body_colors[] = { this->color, this->highlight_color, this->color };

    float radiusT = this->width * 0.500F;
    float radiusB = this->width * 0.125F;
    float radiusY = this->width * 0.020F;
    float body_y = this->height * 0.500F + radiusY;
    float body_height = this->height - body_y - radiusY * 2.0F;

    this->body = geometry_freeze(pyramid_surface(radiusT, radiusB, radiusY, body_height));
    this->body_brush = make_linear_gradient_brush(0.0F, radiusB, this->width, radiusB, MAKE_GRADIENT_STOPS(body_colors));
    
    this->topface = geometry_freeze(ellipse(radiusT, radiusY, radiusT, radiusY));
    this->topface_brush = make_linear_gradient_brush(0.0F, body_y, this->width, body_y, MAKE_GRADIENT_STOPS(topface_colors));
    
    { // particles
        auto nothing = geometry_freeze(blank());
        this->particles_width = this->width * 0.75F;
        this->particles = ref new Vector<CanvasCachedGeometry^>(int(ceil(body_y / 4.0F)), nothing);
    }
}

void Funnellet::fill_extent(float x, float y, float* w, float* h) {
    SET_VALUES(w, this->width, h, this->height);
}

Rect Funnellet::get_inlet() {
    return Rect{ 0.0F, this->topface_brush->EndPoint.y , this->width, 0.0F };
}

Rect Funnellet::get_outlet() {
    float radius = this->body_brush->EndPoint.y;
    float height = radius;

    return Rect{ this->width * 0.5F - radius, this->height - height, radius * 2.0F, height };
}

void Funnellet::update(long long count, long long interval, long long uptime, bool is_slow) {
    auto particles = blank();
    
    srand((unsigned int)uptime);
    for (unsigned int i = 0; i < this->particles->Size; i++) {
        float x = float(rand()) / float(RAND_MAX) * this->particles_width;
        particles = geometry_union(particles, circle(x, 0.0F, 1.0F));
    }

    this->particles->RemoveAt(0);
    this->particles->Append(geometry_freeze(particles));
}

void Funnellet::draw(CanvasDrawingSession^ ds, float x, float y, float Width, float Height) {
    float body_y = y + this->topface_brush->EndPoint.y;

    { // drawing top face after drawing body makes the edge more smoothing.
        brush_translate(this->topface_brush, x, body_y);
        brush_translate(this->body_brush, x, body_y);

        ds->DrawCachedGeometry(this->body, x, body_y, this->body_brush);
        ds->DrawCachedGeometry(this->topface, x, body_y, this->topface_brush);
    }

    { // draw particles
        float particles_interval = 4.0F;
        float particles_x = x + (this->width - this->particles_width) * 0.5F;
        float particles_y = body_y;
        for (unsigned int i = 0; i < this->particles->Size; i++) {
            auto line = this->particles->GetAt(i);
            ds->DrawCachedGeometry(line, particles_x, particles_y, particle_color);
            particles_y -= particles_interval;
        }
    }

    ds->DrawRectangle(this->get_inlet(), Colors::Firebrick);
    ds->DrawRectangle(this->get_outlet(), Colors::Firebrick);
}
