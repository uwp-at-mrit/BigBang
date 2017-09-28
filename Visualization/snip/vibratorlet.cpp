#include <algorithm>

#include "geometry.hpp"
#include "gradient.hpp"
#include "snip/vibratorlet.hpp"

using namespace WarGrey::SCADA;

using namespace Windows::UI;
using namespace Microsoft::Graphics::Canvas;
using namespace Microsoft::Graphics::Canvas::Brushes;

static Color body_color = ColorHelper::FromArgb(255, 50, 50, 50);
static Color hat_color = Colors::DodgerBlue;
static Color hat_decorator_color = Colors::DarkOrange;

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
Vibratorlet::Vibratorlet(float width) : Vibratorlet(width, width * 2.4F) { }

Vibratorlet::Vibratorlet(float width, float height) {
    this->width = width;
    this->height = height;

    setup_gradient_stops();
}

void Vibratorlet::fill_extent(float x, float y, float* w, float* h, float* b, float* t, float* l, float* r) {
    SET_VALUES(w, this->width, h, this->height);
    SET_BOXES(b, t, 0.0F);
    SET_BOXES(l, r, 0.0F);
}

void Vibratorlet::draw(CanvasDrawingSession^ ds, float x, float y, float Width, float Height) {
    float hat_yoff = this->height * 0.12F;
    float hat_height = this->height * 0.20F;
    float hat_width = this->width * 0.38F;
    float body_width = hat_width * 2.0F;
    float body_height = this->height - hat_height - hat_yoff;
    float body_x = x + (this->width - body_width) / 2.0F;
    float body_y = y + this->height - body_height;
    float cx = x + this->width / 2.0F;

    ds->FillRectangle(body_x, body_y, body_width, body_height, body_color);

    { // draw hat and body
        float hat_x = cx - hat_width / 2.0F;
        float hat_bthickness = this->height * 0.005F;
        float hat_bradiusX = hat_width * 0.75F;
        auto hat_brush = make_linear_gradient_brush(this->info, hat_x, y, hat_x + hat_width, y, hat_stops);
        
        ds->FillEllipse(cx, body_y + 1.0F, hat_bradiusX, hat_bthickness, hat_color);
        ds->DrawRectangle(hat_x, body_y - 1.0F, hat_width, 1.0F, body_color);
        ds->FillRectangle(hat_x, y + hat_yoff, hat_width, hat_height, hat_brush);
    }

    { // draw rings
        int defcount = 10;
        int stepunit = 5;
        int count = (body_height > float(stepunit * defcount)) ? defcount : int(std::floor(body_height / float(stepunit)));
        float rx = cx - x;
        float ry = body_height / float(count * stepunit);
        float step = ry * float(stepunit);
        float yoff = body_y + step / 2.0F;
        float box_height = ry * 2.0F;
        auto ring_brush = make_linear_gradient_brush(ds, x, body_y, x + this->width, body_y, ring_stops);

        for (int i = 0; i < count; i++) {
            float cy = yoff + i * step;
            ds->FillEllipse(cx, cy, rx, ry, ring_brush);
            ds->FillRectangle(body_x, cy - box_height, body_width, box_height, body_color);
        }
    }

    ds->DrawRectangle(x, y, this->width, this->height, Colors::Firebrick);
}
