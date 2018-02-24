#include <algorithm>

#include "paint.hpp"
#include "shape.hpp"
#include "colorspace.hpp"
#include "snip/vibratorlet.hpp"

using namespace WarGrey::SCADA;

using namespace Windows::UI;
using namespace Windows::Foundation;
using namespace Microsoft::Graphics::Canvas;

static const float default_ratio = 2.0F;

static Color body_color = rgba(0x323232);
static Color hat_color = Colors::DodgerBlue;
static Color hat_decorator_color = Colors::DarkOrange;
static Color hat_decorator_mid_color = Colors::Orange;
static Color hat_decorator_midbg_color = rgba(0xFFBE00);

/*************************************************************************************************/
Vibratorlet::Vibratorlet(float width, float height) : width(width), height(height) {
    if (height == 0.0F) {
        this->height = width * default_ratio;
    } else if (height < 0.0F) {
        this->height = -(width * height);
    }
}

void Vibratorlet::construct() {
    this->initialize_hat();
    this->initialize_rings();
}

void Vibratorlet::initialize_hat() {
    Color hat_colors[] = { hat_color, hat_color, Colors::Silver, hat_color, hat_color, hat_color };
    
    double d_angle = 30.0;
    float hat_height = this->height * 0.20F;
    float hat_width = this->width * 0.28F;
    float hat_bottom_y = hat_height - 1.0F;
    float hat_bthickness = this->height * 0.005F;
    float hat_bradiusX = hat_width * 0.8F - hat_bthickness;
    float hat_bradiusY = hat_bthickness * 0.5F;

    float end_width = hat_width * 0.6F;
    float mid_width = hat_width * 0.5F;
    float end_height = hat_height * 0.3F;
    float mid_height = end_height * 1.2F;
    float back_width = std::fmax(end_width, mid_width);
    float back_height = end_height + mid_height + end_height;
    float box_x = (hat_width - back_width) * 0.5F;
    float box_y = - hat_height / 17.0F;
    float mid_x = (hat_width - mid_width) * 0.5F;
    float mid_y = box_y + end_height;

    auto hat = rectangle(hat_width, hat_height);
    auto bottom = long_arc(hat_width, hat_bottom_y, 0.0F, hat_bottom_y, hat_bradiusX, hat_bradiusY, hat_bthickness);
    auto backend = geometry_rotate(rounded_rectangle(box_x, box_y, back_width, back_height, -0.04F, -0.01F), d_angle);

    auto topbox = rounded_rectangle(box_x, box_y, end_width, end_height, -0.03F, -0.04F);
    auto bottombox = rounded_rectangle(box_x, mid_y + mid_height, end_width, end_height, -0.04F, -0.03F);
    auto frontend = geometry_rotate(geometry_union(topbox, bottombox), -d_angle);

    float midfg_width = mid_width - (mid_x - box_x) * 2.0F;
    float midfg_height = mid_height / 1.5F;
    float midfg_x = (hat_width - midfg_width) * 0.5F;
    float midfg_y = mid_y + (mid_height - midfg_height) * 0.5F;

    auto midbox = rotate_rectangle(mid_x, mid_y, mid_width, mid_height, -d_angle);
    auto midfg = geometry_rotate(rounded_rectangle(midfg_x, midfg_y, midfg_width, midfg_height, -0.04F, -0.03F), -d_angle);
    auto hollow_midbg = geometry_subtract(midbox, midfg);

    this->hat_adjust_yoff = -(backend->ComputeBounds()).Y;
    this->hat_decorator_sides = geometry_freeze(geometry_union(frontend, geometry_subtract(backend, hat)));
    this->hat_bottom = geometry_freeze(bottom);
    this->hat = geometry_subtract(geometry_subtract(hat, frontend), midbox);
    this->hat_frontend_midbg = geometry_freeze(hollow_midbg);
    this->hat_frontend_midfg = geometry_freeze(midfg);
    this->hat_brush = make_linear_gradient_brush(hat_width, 0.0F, MAKE_GRADIENT_STOPS(hat_colors));
}

void Vibratorlet::initialize_rings() { // draw body and rings
    Color ring_colors[] = { Colors::Gray, Colors::Silver, Colors::Gray, Colors::Gray };

    Rect hat_bounds = this->hat->ComputeBounds();
    float body_width = hat_bounds.Width * 2.0F;
    float body_height = this->height - hat_bounds.Height - this->hat_adjust_yoff;

    int defcount = 10;
    int stepunit = 5;
    int count = (body_height > float(stepunit * defcount)) ? defcount : int(std::floor(body_height / float(stepunit)));
    float thickness = body_height / float(count * stepunit);
    float rx = this->width * 0.5F - thickness;
    float ry = thickness / 2.0F;
    auto rings = blank();

    this->ring_interval = thickness * float(stepunit);
    for (int i = 0; i < count; i++) {
        float cy = i * this->ring_interval;
        rings = geometry_union(rings, long_arc(body_width, cy, 0.0F, cy, rx, ry, thickness));
    }

    this->body = rectangle(body_width, body_height);
    this->rings = geometry_freeze(rings);
    this->ring_brush = make_linear_gradient_brush(this->width, 0.0F, MAKE_GRADIENT_STOPS(ring_colors));
}

void Vibratorlet::fill_extent(float x, float y, float* w, float* h) {
    SET_VALUES(w, this->width, h, this->height);
}

void Vibratorlet::update(long long count, long long interval, long long uptime, bool is_slow) {
    this->vibrated = (count % 2 == 0);
}

void Vibratorlet::draw(CanvasDrawingSession^ ds, float x, float y, float Width, float Height) {
    Rect body_bounds = this->body->ComputeBounds();
    Rect hat_bounds = this->hat->ComputeBounds();

    { // draw body and rings
        float body_x = x + (this->width - body_bounds.Width) * 0.5F;
        float body_y = y + this->height - body_bounds.Height;
        float rings_y = body_y + this->ring_interval / (this->vibrated ? 4.0F : 2.0F);

        brush_translate(this->ring_brush, body_x, 0.0F);
        ds->FillGeometry(this->body, body_x, body_y, body_color);
        ds->DrawCachedGeometry(this->rings, body_x, rings_y, this->ring_brush);
    }

    { // draw hat and decorators
        float hat_x = x + (this->width - hat_bounds.Width) * 0.5F;
        float hat_y = y + this->hat_adjust_yoff;

        brush_translate(this->hat_brush, hat_x, 0.0F);
        ds->FillGeometry(this->hat, hat_x, hat_y, this->hat_brush);
        ds->DrawCachedGeometry(this->hat_decorator_sides, hat_x, hat_y, hat_decorator_color);
        ds->DrawCachedGeometry(this->hat_bottom, hat_x, hat_y, hat_color);
        ds->DrawCachedGeometry(this->hat_frontend_midbg, hat_x, hat_y, hat_decorator_midbg_color);
        ds->DrawCachedGeometry(this->hat_frontend_midfg, hat_x, hat_y, hat_decorator_mid_color);
    }
}
