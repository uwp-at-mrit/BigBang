﻿#include "paint.hpp"
#include "shape.hpp"
#include "colorspace.hpp"
#include "snip/pipeline/screwlet.hpp"
#include "snip/pipeline/static.hpp"

using namespace WarGrey::SCADA;

using namespace Windows::UI;
using namespace Windows::Foundation;
using namespace Windows::Foundation::Numerics;

using namespace Microsoft::Graphics::Canvas;
using namespace Microsoft::Graphics::Canvas::Geometry;

Screwlet::Screwlet(float width, float height, float thickness, double color, double saturation, double light, double highlight)
    : width(width), height(height), pipe_thickness(thickness) {
    if (thickness == 0.0F) {
        this->pipe_thickness = this->width * default_pipe_thickness_ratio;
    } else if (thickness < 0.0F) {
        this->pipe_thickness = -this->width * thickness;
    }

    this->fitting_width = this->pipe_thickness * 0.25F;

    this->color = hsla(color, saturation, light);
    this->highlight_color = hsla(color, saturation, highlight);
    this->fitting_color = hsla(color, saturation, light * default_connector_light_rate);
    this->body_color = hsla(color, saturation, light * default_body_light_rate);
    this->base_color = hsla(color, saturation, light * default_endpoint_light_rate);
}

void Screwlet::load() {
    Color fitting_colors[] = { this->fitting_color, this->highlight_color, this->fitting_color, this->fitting_color };
    Color pipe_colors[] = { this->color, this->highlight_color, this->color, this->color };
    auto fitting_stops = MAKE_GRADIENT_STOPS(fitting_colors);
    auto pipe_stops = MAKE_GRADIENT_STOPS(pipe_colors);

    float ascent = this->pipe_thickness * 0.5F;
    float fitting_ry = this->pipe_thickness * 0.5F + ascent;
    float fitting_rx = fitting_ry * 0.0618F;
    float base_width = this->pipe_thickness * 1.618F;
    float base_height = (base_width - this->pipe_thickness) * 0.5F;
    float pipe_x = base_height + this->pipe_thickness;
    
    this->fitting_brush = make_linear_gradient_brush(fitting_rx, 0.0F, fitting_rx, fitting_ry * 2.0F, fitting_stops);
    this->pipe_brush = make_linear_gradient_brush(pipe_x, ascent, pipe_x, this->pipe_thickness + ascent, pipe_stops);
    this->fitting = geometry_freeze(cylinder_rl_surface(fitting_rx, fitting_ry, this->fitting_width));
}

void Screwlet::fill_extent(float x, float y, float* w, float* h) {
    SET_VALUES(w, this->width, h, this->height);
}

Rect Screwlet::get_inlet() {
    float pipe_x = this->pipe_brush->StartPoint.x;
    float pipe_length = this->width - pipe_x;
    float port_width = pipe_length * 0.618F;
    float x = pipe_x + (pipe_length - port_width) * 0.5F;

    return Rect{ x, this->pipe_brush->StartPoint.y, port_width, 0.0F };
}

Rect Screwlet::get_outlet() {
    return Rect{ this->width, this->pipe_brush->StartPoint.y, 0.0F, this->pipe_thickness };
}

void Screwlet::draw(CanvasDrawingSession^ ds, float x, float y, float Width, float Height) {
    float base_height = this->pipe_brush->StartPoint.x - this->pipe_thickness;
    float base_width = base_height * 2.0F + this->pipe_thickness;
    float body_x = x + base_height;
    float body_y = y + base_height;
    float body_height = this->height - body_y + y;
    float body_corner = base_height * 0.5F;
    float base_x = body_x - (base_width - this->pipe_thickness) * 0.5F;
    float base_y = y + this->height - base_height;

    ds->FillRoundedRectangle(body_x, body_y, this->pipe_thickness, body_height, body_corner, body_corner, this->body_color);
    ds->FillRectangle(base_x, base_y, base_width, base_height, this->base_color);

    { // draw pipe
        float connector_rx = this->fitting_brush->StartPoint.x;
        float pipe_x = body_x + this->pipe_thickness;
        float pipe_y = y + this->pipe_brush->StartPoint.y + this->pipe_thickness * 0.5F;
        float pipe_xoff = this->fitting_width - connector_rx * 0.1618F;
        float cx = pipe_x + pipe_xoff;

        brush_translate(this->fitting_brush, x, y);
        brush_translate(this->pipe_brush, x, y);
        ds->FillEllipse(cx, pipe_y, connector_rx, pipe_y - y, this->color);
        ds->DrawLine(cx, pipe_y, x + this->width, pipe_y, this->pipe_brush, this->pipe_thickness);
        ds->DrawCachedGeometry(this->fitting, pipe_x - connector_rx, y, this->fitting_brush);
    }
}
