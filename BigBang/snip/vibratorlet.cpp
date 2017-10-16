#include <algorithm>

#include "rsyslog.hpp"
#include "geometry.hpp"
#include "gradient.hpp"
#include "snip/vibratorlet.hpp"

using namespace WarGrey::SCADA;

using namespace Windows::UI;
using namespace Windows::Foundation;

using namespace Microsoft::Graphics::Canvas;
using namespace Microsoft::Graphics::Canvas::Brushes;

static Color body_color = ColorHelper::FromArgb(255, 50, 50, 50);
static Color hat_color = Colors::DodgerBlue;
static Color hat_decorator_color = Colors::DarkOrange;
static Color hat_decorator_mid_color = Colors::Orange;
static Color hat_decorator_midbg_color = ColorHelper::FromArgb(255, 255, 190, 0);

static Color hat_colors[] = { hat_color, hat_color, Colors::Silver, hat_color, hat_color, hat_color };
static Color ring_colors[] = { Colors::Gray, Colors::Silver, Colors::Gray, Colors::Gray };

static Platform::Array<CanvasGradientStop>^ hat_stops = nullptr;
static Platform::Array<CanvasGradientStop>^ ring_stops = nullptr;

static void setup_gradient_stops() {
    if (hat_stops == nullptr) {
        hat_stops = MAKE_GRADIENT_STOPS(hat_colors);
        ring_stops = MAKE_GRADIENT_STOPS(ring_colors);
    }
}

/*************************************************************************************************/
Vibratorlet::Vibratorlet(float width) : Vibratorlet(width, width * 2.0F) {}

Vibratorlet::Vibratorlet(float width, float height) {
    this->width = width;
    this->height = height;
}

void Vibratorlet::load() {
    setup_gradient_stops();
}

void Vibratorlet::fill_extent(float x, float y, float* w, float* h, float* b, float* t, float* l, float* r) {
    SET_VALUES(w, this->width, h, this->height);
    SET_BOXES(b, t, 0.0F);
    SET_BOXES(l, r, 0.0F);
}

void Vibratorlet::update(long long count, long long interval, long long uptime, bool is_slow) {
    this->vibrated = (count % 2 == 0);
}

void Vibratorlet::draw(CanvasDrawingSession^ ds, float x, float y, float Width, float Height) {
    double d_angle = 30.0;
    float hat_yoff = this->height * 0.02F; // TODO: this rate should based on d_angle.
    float hat_height = this->height * 0.20F;
    float hat_width = this->width / 3.6F;
    float body_width = hat_width * 2.0F;
    float body_height = this->height - hat_height - hat_yoff;
    float body_x = x + (this->width - body_width) / 2.0F;
    float body_y = y + this->height - body_height;
    float cx = x + this->width / 2.0F;

    { // draw body and rings
        int defcount = 10;
        int stepunit = 5;
        int count = (body_height > float(stepunit * defcount)) ? defcount : int(std::floor(body_height / float(stepunit)));
        float thickness = body_height / float(count * stepunit);
        float rx = cx - x - thickness;
        float ry = thickness / 2.0F;
        float step = thickness * float(stepunit);
        float yoff = body_y + step / (this->vibrated ? 2.0F : 1.0F);

        auto body = rectangle(body_x, body_y, body_width, body_height);
        auto ring_brush = make_linear_gradient_brush(x, body_y, x + this->width, body_y, ring_stops);
        auto rings = blank();
        for (int i = 0; i < count; i++) {
            float cy = yoff + i * step;
            rings = geometry_union(rings, long_arc(body_x + body_width, cy, body_x, cy, rx, ry, thickness));
        }

        ds->FillGeometry(geometry_substract(body, rings), body_color);
        ds->FillGeometry(rings, ring_brush);
    }

    { // draw hat and decorators
        float hat_x = cx - hat_width / 2.0F;
        float hat_y = y + hat_yoff;
        float hat_by = body_y - 1.0F;
        float hat_bthickness = this->height * 0.005F;
        float hat_bradiusX = hat_width * 0.8F - hat_bthickness;
        float hat_bradiusY = hat_bthickness / 2.0F;
        
        float end_width = hat_width * 0.6F;
        float mid_width = hat_width * 0.5F;
        float end_height = hat_height / 3.4F;
        float mid_height = end_height * 1.2F;
        float back_width = std::fmax(end_width, mid_width);
        float back_height = end_height + mid_height + end_height;
        float box_x = cx - back_width / 2.0F;
        float box_y = hat_y - hat_height / 17.0F;
        float mid_x = cx - mid_width / 2.0F;
        float mid_y = box_y + end_height;
        
        auto hat_brush = make_linear_gradient_brush(hat_x, y, hat_x + hat_width, y, hat_stops);
        auto hat = rectangle(hat_x, hat_y, hat_width, hat_height);
        auto hat_bottom = long_arc(hat_x + hat_width, hat_by, hat_x, hat_by, hat_bradiusX, hat_bradiusY, hat_bthickness);
        auto backend = geometry_rotate(rounded_rectangle(box_x, box_y, back_width, back_height, -0.04F, -0.01F), d_angle);

        auto topbox = rounded_rectangle(box_x, box_y, end_width, end_height, -0.03F, -0.04F);
        auto bottombox = rounded_rectangle(box_x, mid_y + mid_height, end_width, end_height, -0.04F, -0.03F);
        auto frontend = geometry_rotate(geometry_union(topbox, bottombox), -d_angle);

        float midfg_width = mid_width - (mid_x - box_x) * 2.0F;
        float midfg_height = mid_height / 1.5F;
        float midfg_x = cx - midfg_width / 2.0F;
        float midfg_y = mid_y + (mid_height - midfg_height) / 2.0F;
        auto midbox = rotate_rectangle(mid_x, mid_y, mid_width, mid_height, -d_angle);
        auto midfg = geometry_rotate(rounded_rectangle(midfg_x, midfg_y, midfg_width, midfg_height, -0.04F, -0.03F), -d_angle);
        auto hollow_midbg = geometry_substract(midbox, midfg);

        ds->FillGeometry(geometry_substract(backend, hat), hat_decorator_color);
        ds->FillGeometry(hat_bottom, hat_color);
        ds->FillGeometry(geometry_substract(geometry_substract(hat, frontend), midbox), hat_brush);
        ds->FillGeometry(frontend, hat_decorator_color);
        ds->FillGeometry(hollow_midbg, hat_decorator_midbg_color);
        ds->FillGeometry(midfg, hat_decorator_mid_color);
    }
}
