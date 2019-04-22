#include <algorithm>

#include "graphlet/device/motorlet.hpp"

#include "shape.hpp"
#include "paint.hpp"
#include "brushes.hxx"

using namespace WarGrey::SCADA;

using namespace Microsoft::Graphics::Canvas;
using namespace Microsoft::Graphics::Canvas::Brushes;

static const float default_ratio = 0.6F;
static CanvasSolidColorBrush^ dark_color = Colours::Gray;
static CanvasSolidColorBrush^ light_color = Colours::Silver;

/*************************************************************************************************/
Motorlet::Motorlet(float width, float height) : width(width), height(height) {
    if (height == 0.0F) {
        this->height = width * default_ratio;
    } else if (height < 0.0F) {
        this->height = -(width * height);
    }
}

void Motorlet::construct() {
	CanvasSolidColorBrush^ serew_colors[] = { Colours::White, Colours::Black };
	CanvasSolidColorBrush^ body_colors[] = {
        dark_color, light_color, light_color, light_color,
        light_color, light_color, light_color, dark_color
    };

    auto serew_stops = make_gradient_stops(serew_colors); // don't mind, it's Visual Studio's fault.
    auto body_stops = make_gradient_stops(body_colors);   // don't mind, it's Visual Studio's fault.

    float thread = fmaxf(this->width * 0.01F, 1.0F);
    float body_height = this->height * 0.97F;
    float body_y = this->height - body_height;
    float serew_x = this->width * 0.8F;

    this->serew_brush = make_linear_gradient_brush(serew_x, serew_x, serew_x + thread, serew_x - thread, serew_stops);

    { // body and parts
        float head_height = body_height * 0.8F;
        float head_width = this->width * 0.10F;
        float body_width = this->width * 0.54F;
        float tail_width = this->width * 0.16F;
        float head_bar_x = serew_x - head_width;
        float head_x = serew_x - head_width * 1.25F;
        float head_y = body_y + (body_height - head_height) * 0.5F;
        float body_x = tail_width;

        auto head_part = rounded_rectangle(head_x, head_y, serew_x - head_x, head_height, -0.10F, -0.06F);
        auto body_part = rectangle(body_x, body_y, body_width, body_height);
        auto tail_part = rounded_rectangle(0.0F, body_y, tail_width * 1.25F, body_height, -0.25F, -0.10F);

        auto lines = blank(); {
            int defcount = 8;
            int stepunit = 2;
            int count = (head_height > float(stepunit * defcount)) ? defcount : int(floor(head_height / float(stepunit)));
            float thickness = head_height / float(count * stepunit);
            float end_x = body_x + body_width;
            float step = thickness * float(stepunit);
            float yoff = head_y + step / 2.0F;

            for (int i = 0; i < count; i++) {
                float end_y = yoff + i * step;
                lines = geometry_union(lines, hline(body_x, end_y, body_width, thickness));
            }
        }

        { // body parts
            float small_box_size = body_y * 4.0F;
            float small_box_x = body_x + body_y;
            float bar_x = small_box_x + small_box_size + body_y;
            float bar_width = body_width - small_box_size - body_y * 4.0F;
            float bar_height = body_y * 3.0F;
            float radius = small_box_size * (1.0F - 0.618F);
            float centeroff = small_box_size / 2.0F;

            auto bar = rectangle(bar_x, 0.0F, bar_width, bar_height);
            auto small_box = rectangle(small_box_x, body_y, small_box_size, small_box_size);
            auto small_status = circle(small_box_x + centeroff, body_y + centeroff, radius);
            auto small_background = geometry_subtract(small_box, small_status);

            { // motor status
                float box_size = bar_width * 0.618F;
                float box_x = bar_x + (bar_width - box_size) / 2.0F;
                float box_y = this->height - box_size;
                float status_size = (body_y <= 1.0F) ? box_size : (box_size - body_y * 4.0F);
                float status_x = bar_x + (bar_width - status_size) / 2.0F;
                float status_y = box_y + (box_size - status_size) / 2.0F;

                auto box = rectangle(box_x, box_y, box_size, box_size);
                auto status = rectangle(status_x, status_y, status_size, status_size);
                auto background = geometry_union(geometry_subtract(box, status), small_background);

                { // TODO: is it really more efficient to make the body hollow?
                    auto hollow_lines = geometry_subtract(lines, box);
                    auto body_masks = geometry_union(geometry_union(small_box, box), geometry_union(bar, lines));
                    auto hollow_body = geometry_subtract(body_part, body_masks);

                    this->head = geometry_freeze(geometry_subtract(head_part, body_part));
                    this->body = geometry_freeze(geometry_union(tail_part, hollow_body));
                    this->lines = geometry_freeze(geometry_union(small_status, hollow_lines));
                }

                { // body parts
                    auto outline = geometry_union(bar, background);

                    this->outline = geometry_draft(outline);
                    this->parts = geometry_freeze(outline);
                    this->status = geometry_freeze(status);
                }
            }
        }

        this->head_brush = make_linear_gradient_brush(head_bar_x, head_y, head_bar_x, head_y + head_height, body_stops);
        this->body_brush = make_linear_gradient_brush(body_x, body_y, body_x, body_y + body_height, body_stops);
    }
}

void Motorlet::fill_extent(float x, float y, float* w, float* h) {
    SET_VALUES(w, this->width, h, this->height);
}

void Motorlet::update(long long count, long long interval, long long uptime) {
    brush_translate(this->serew_brush, float(count), 0.0F);
	this->notify_updated();
}

void Motorlet::draw(CanvasDrawingSession^ ds, float x, float y, float Width, float Height) {
    float body_y = y + this->body_brush->StartPoint.y;
    float body_height = this->body_brush->EndPoint.y - this->body_brush->StartPoint.y;
    
    { // draw serew
        float head_height = this->head_brush->EndPoint.y - this->head_brush->StartPoint.y;
        float serew_x = x + this->serew_brush->StartPoint.x;
        float serew_y = body_y + body_height * 0.5F;
        float thickness = this->height * 0.12F;

        ds->DrawLine(serew_x, serew_y, x + this->width, serew_y, this->serew_brush, thickness);
    }

    { // draw body
        float head_x = x + this->head_brush->StartPoint.x;
        float body_x = x + this->body_brush->StartPoint.x;
    
        brush_translate(this->head_brush, x, y);
        brush_translate(this->body_brush, x, y);
        ds->DrawCachedGeometry(this->head, x, y, this->head_brush);
        ds->DrawCachedGeometry(this->body, x, y, this->body_brush);
        ds->DrawCachedGeometry(this->lines, x, y, dark_color);
        ds->DrawLine(body_x, body_y, body_x, body_y + body_height, Colours::DimGray);
        ds->DrawLine(head_x, body_y, head_x, body_y + body_height, Colours::DimGray);
    }

    { // draw body parts
        ds->DrawCachedGeometry(this->parts, x, y, light_color);
        ds->DrawCachedGeometry(this->outline, x, y, dark_color);
        ds->DrawCachedGeometry(this->status, x, y, Colours::Green);
    }
}
