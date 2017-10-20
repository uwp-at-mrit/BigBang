﻿#include <algorithm>

#include "geometry.hpp"
#include "gradient.hpp"
#include "snip/motorlet.hpp"

using namespace WarGrey::SCADA;

using namespace Windows::UI;
using namespace Microsoft::Graphics::Canvas;

static Color dark_color = Colors::Gray;
static Color light_color = Colors::Silver;

/*************************************************************************************************/
Motorlet::Motorlet(float width) : Motorlet(width, width * 0.6F) {}
Motorlet::Motorlet(float width, float height) : width(width), height(height) {}

void Motorlet::load() {
    Color screw_colors[] = { Colors::White, Colors::Black };
    Color body_colors[] = {
        dark_color, light_color, light_color, light_color,
        light_color, light_color, light_color, dark_color
    };

    auto screw_stops = MAKE_GRADIENT_STOPS(screw_colors);
    auto body_stops = MAKE_GRADIENT_STOPS(body_colors);

    float thread = std::fmax(this->width * 0.01F, 1.0F);
    float body_height = this->height * 0.97F;
    float body_y = this->height - body_height;
    float screw_x = this->width * 0.8F;

    this->screw_brush = make_linear_gradient_brush(screw_x, screw_x, screw_x + thread, screw_x - thread, screw_stops);

    { // body and parts
        float head_height = body_height * 0.8F;
        float head_width = this->width * 0.10F;
        float body_width = this->width * 0.54F;
        float tail_width = this->width * 0.16F;
        float head_bar_x = screw_x - head_width;
        float head_x = screw_x - head_width * 1.25F;
        float head_y = body_y + (body_height - head_height) * 0.5F;
        float body_x = tail_width;

        auto head_part = rounded_rectangle(head_x, head_y, screw_x - head_x, head_height, -0.10F, -0.06F);
        auto body_part = rectangle(body_x, body_y, body_width, body_height);
        auto tail_part = rounded_rectangle(0.0F, body_y, tail_width * 1.25F, body_height, -0.25F, -0.10F);

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
            auto small_background = geometry_substract(small_box, small_status);

            { // motor status
                float box_size = bar_width * 0.618F;
                float box_x = bar_x + (bar_width - box_size) / 2.0F;
                float box_y = this->height - box_size;
                float status_size = (body_y <= 1.0F) ? box_size : (box_size - body_y * 4.0F);
                float status_x = bar_x + (bar_width - status_size) / 2.0F;
                float status_y = box_y + (box_size - status_size) / 2.0F;

                auto box = rectangle(box_x, box_y, box_size, box_size);
                auto status = rectangle(status_x, status_y, status_size, status_size);
                auto background = geometry_union(geometry_substract(box, status), small_background);

                { // TODO: is it really more efficient to make the body hollow?
                    auto hollow_lines = geometry_substract(lines, box);
                    auto body_masks = geometry_union(geometry_union(small_box, box), geometry_union(bar, lines));
                    auto hollow_body = geometry_substract(body_part, body_masks);

                    this->head = geometry_freeze(geometry_substract(head_part, body_part));
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

void Motorlet::fill_extent(float x, float y, float* w, float* h, float* b, float* t, float* l, float* r) {
    SET_VALUES(w, this->width, h, this->height);
    SET_BOXES(b, t, 0.0F);
    SET_BOXES(l, r, 0.0F);
}

void Motorlet::update(long long count, long long interval, long long uptime, bool is_slow) {
    brush_translate(this->screw_brush, float(count), 0.0F);
}

void Motorlet::draw(CanvasDrawingSession^ ds, float x, float y, float Width, float Height) {
    float body_y = y + this->body_brush->StartPoint.y;
    float body_height = this->body_brush->EndPoint.y - this->body_brush->StartPoint.y;
    
    { // draw screw
        float head_height = this->head_brush->EndPoint.y - this->head_brush->StartPoint.y;
        float screw_x = x + this->screw_brush->StartPoint.x;
        float screw_y = body_y + body_height * 0.5F;
        float thickness = this->height / 7.5F;

        ds->DrawLine(screw_x, screw_y, x + this->width, screw_y, this->screw_brush, thickness);
    }

    { // draw body
        float head_x = x + this->head_brush->StartPoint.x;
        float body_x = x + this->body_brush->StartPoint.x;
    
        brush_translate(this->head_brush, x, y);
        brush_translate(this->body_brush, x, y);
        ds->DrawCachedGeometry(this->head, x, y, this->head_brush);
        ds->DrawCachedGeometry(this->body, x, y, this->body_brush);
        ds->DrawCachedGeometry(this->lines, x, y, dark_color);
        ds->DrawLine(body_x, body_y, body_x, body_y + body_height, Colors::DimGray);
        ds->DrawLine(head_x, body_y, head_x, body_y + body_height, Colors::DimGray);
    }

    { // draw body parts
        ds->DrawCachedGeometry(this->parts, x, y, light_color);
        ds->DrawCachedGeometry(this->outline, x, y, dark_color);
        ds->DrawCachedGeometry(this->status, x, y, Colors::Green);
    }
}
