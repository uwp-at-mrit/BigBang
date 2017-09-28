#include <algorithm>

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
Vibratorlet::Vibratorlet(float width) : Vibratorlet(width, width * 1.95F) {}

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
    float hat_width = this->width / 3.6F;
    float body_width = hat_width * 2.0F;
    float body_height = this->height - hat_height - hat_yoff;
    float body_x = x + (this->width - body_width) / 2.0F;
    float body_y = y + this->height - body_height;
    float cx = x + this->width / 2.0F;

    ds->FillRectangle(body_x, body_y, body_width, body_height, body_color);

    { // draw hat and body
        float hat_x = cx - hat_width / 2.0F;
        float hat_y = y + hat_yoff;
        float hat_bthickness = this->height * 0.005F;
        float hat_bradiusX = hat_width * 0.8F - hat_bthickness / 2.0F;
        float hat_bradiusY = hat_bthickness - 1.5F;
        
        float endpart_width = hat_width * 0.6F;
        float midpart_width = hat_width * 0.5F;
        float endpart_height = hat_height / 3.4F;
        float midpart_height = endpart_height * 1.2F;
        float bewidth = std::fmax(endpart_width, midpart_width);
        float beheight = endpart_height + midpart_height + endpart_height;
        float dx = cx - bewidth / 2.0F;
        float dy = hat_y - hat_height / 17.0F;
        float dcy = hat_y + hat_height * 0.43F;

        auto hat_path = rectangle(hat_x, hat_y, hat_width, hat_height);
        auto be_path = geometry_rotate(rectangle(dx, hat_y, bewidth, beheight), 30.0);
        auto hat_brush = make_linear_gradient_brush(hat_x, y, hat_x + hat_width, y, hat_stops);
        auto fe = rotate_rectangle(dx, hat_y, bewidth, beheight, 30.0, cx, dcy);

        ds->FillGeometry(geometry_substract(be_path, hat_path), hat_decorator_color);
        ds->DrawEllipse(cx, body_y, hat_bradiusX, hat_bradiusY, hat_color, hat_bthickness);
        //ds->DrawRectangle(hat_x, body_y - 1.0F, hat_width, 1.0F, body_color);
        ds->FillRectangle(hat_x, hat_y, hat_width, hat_height, hat_brush);
    }

    { // draw rings
        int defcount = 10;
        int stepunit = 5;
        int count = (body_height > float(stepunit * defcount)) ? defcount : int(std::floor(body_height / float(stepunit)));
        float thickness = body_height / float(count * stepunit);
        float rx = cx - x - thickness;
        float ry = thickness - 1.5F;
        float step = thickness * float(stepunit);
        float yoff = body_y + step / 2.0F;
        float box_height = thickness * 2.0F;
        auto ring_brush = make_linear_gradient_brush(x, body_y, x + this->width, body_y, ring_stops);

        for (int i = 0; i < count; i++) {
            float cy = yoff + i * step;
            ds->DrawEllipse(cx, cy, rx, ry, ring_brush, thickness);
            ds->FillRectangle(body_x, cy - box_height, body_width, box_height, body_color);
        }
    }

    ds->DrawRectangle(x, y, this->width, this->height, Colors::Firebrick);
}
