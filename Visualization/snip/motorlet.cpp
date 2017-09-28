#include <algorithm>

#include "geometry.hpp"
#include "gradient.hpp"
#include "snip/motorlet.hpp"

using namespace WarGrey::SCADA;

using namespace Windows::UI;
using namespace Microsoft::Graphics::Canvas;
using namespace Microsoft::Graphics::Canvas::Brushes;

static Color dark_color = Colors::Gray;
static Color light_color = Colors::Silver;

static Color body_colors[] = {
    dark_color, light_color, light_color, light_color,
    light_color, light_color, light_color, dark_color
};

static Color screw_colors[] = { Colors::White, Colors::Black };

static Platform::Array<CanvasGradientStop>^ body_stops = nullptr;
static Platform::Array<CanvasGradientStop>^ screw_stops = nullptr;

static void setup_gradient_stops() {
    if (body_stops == nullptr) {
        body_stops = MAKE_GRADIENT_STOPS(body_colors);
        screw_stops = MAKE_GRADIENT_STOPS(screw_colors);
    }
}

/*************************************************************************************************/
Motorlet::Motorlet(float width) : Motorlet(width, width * 0.6F) { }

Motorlet::Motorlet(float width, float height) {
    this->width = width;
    this->height = height;

    setup_gradient_stops();
}

void Motorlet::fill_extent(float x, float y, float* w, float* h, float* b, float* t, float* l, float* r) {
    SET_VALUES(w, this->width, h, this->height);
    SET_BOXES(b, t, 0.0F);
    SET_BOXES(l, r, 0.0F);
}

void Motorlet::draw(CanvasDrawingSession^ ds, float x, float y, float Width, float Height) {
    float screw_x = x + this->width * 0.8F;
    float body_height = this->height * 0.97F;
    float body_yoff = this->height - body_height;
    float body_y = y + body_yoff;
    
    { // draw screw
        float thickness = this->height / 7.5F;
        float thread = std::fmax(this->width * 0.01F, 1.0F);
        float screw_y = body_y + body_height / 2.0F;

        auto screw_brush = make_linear_gradient_brush(screw_x, screw_y, screw_x + thread, screw_y - thread, screw_stops);
        ds->DrawLine(screw_x, screw_y, x + width, screw_y, screw_brush, thickness);
    }

    { // draw the rest
        float head_height = body_height * 0.8F;
        float head_radiusX = this->width * 0.02F;
        float head_radiusY = this->height * 0.03F;
        float tail_radiusX = head_radiusX * 3.0F;
        float tail_radiusY = head_radiusY * 3.0F;

        float body_xoff = this->width * 0.16F;
        float body_width = this->width * 0.54F;

        float body_x = x + body_xoff;
        float head_x = body_x + body_width - head_radiusX;
        float head_y = body_y + (body_height - head_height) / 2.0F;
        float head_width = screw_x - head_x;

        auto head_brush = make_linear_gradient_brush(head_x, head_y, head_x, head_y + head_height, body_stops);
        auto body_brush = make_linear_gradient_brush(x, body_y, x, body_y + body_height, body_stops);

        auto head_part = rounded_rectangle(head_x, head_y, head_width, head_height, -0.02F, -0.03F);
        auto body_part = rectangle(body_x, body_y, body_width, body_height);
        auto tail_part = rounded_rectangle(x, body_y, body_xoff + tail_radiusX, body_height, -0.06F, -0.09F);

        auto lines = blank(); {
            int defcount = 8;
            int stepunit = 2;
            int count = (head_height > float(stepunit * defcount)) ? defcount : int(std::floor(head_height / float(stepunit)));
            float thickness = head_height / float(count * stepunit);
            float end_x = body_x + body_width;
            float step = thickness * float(stepunit);
            float yoff = head_y + step / 2.0F;

            for (int i = 0; i < count; i++) {
                float end_y = yoff + i * step;
                lines = geometry_union(lines, hline(body_x, end_y, body_width, thickness));
            }
        }

        { // prepare for body components
            float small_box_size = body_yoff * 4.0F;
            float small_box_x = body_x + body_yoff;
            float bar_x = small_box_x + small_box_size + body_yoff;
            float bar_width = body_width - small_box_size - body_yoff * 4.0F;
            float bar_height = body_yoff * 3.0F;
            float radius = small_box_size * (1.0F - 0.618F);
            float centeroff = small_box_size / 2.0F;
            
            auto bar = rectangle(bar_x, y, bar_width, bar_height);
            auto small_box = rectangle(small_box_x, body_y, small_box_size, small_box_size);
            auto small_status = circle(small_box_x + centeroff, body_y + centeroff, radius);
            auto small_background = geometry_substract(small_box, small_status);

            { // prepare for motor status
                float box_size = bar_width * 0.618F;
                float box_x = bar_x + (bar_width - box_size) / 2.0F;
                float box_y = y + this->height - box_size;
                float status_size = (body_yoff <= 1.0F) ? box_size : (box_size - body_yoff * 4.0F);
                float status_x = bar_x + (bar_width - status_size) / 2.0F;
                float status_y = box_y + (box_size - status_size) / 2.0F;

                auto box = rectangle(box_x, box_y, box_size, box_size);
                auto status = rectangle(status_x, status_y, status_size, status_size);
                auto background = geometry_union(geometry_substract(box, status), small_background);

                { // draw body
                    auto hollow_lines = geometry_substract(lines, box);
                    auto body_masks = geometry_union(geometry_union(small_box, box), geometry_union(bar, lines));
                    auto hollow_body = geometry_substract(body_part, body_masks);

                    ds->FillGeometry(geometry_substract(head_part, body_part), head_brush);
                    ds->FillGeometry(geometry_substract(tail_part, body_part), body_brush);
                    ds->FillGeometry(hollow_body, body_brush);
                    ds->FillGeometry(hollow_lines, dark_color);
                    ds->DrawLine(body_x, body_y, body_x, body_y + body_height, Colors::DimGray);
                    ds->DrawLine(body_x + body_width, body_y, body_x + body_width, body_y + body_height, Colors::DimGray);
                }

                { // draw body components
                    auto background_outline = geometry_union(bar, background);

                    ds->FillGeometry(background_outline, light_color);
                    ds->DrawGeometry(background_outline, dark_color);

                    ds->FillGeometry(small_status, dark_color);
                    ds->FillGeometry(status, Colors::Green);
                }
            }
        }
    }
}
