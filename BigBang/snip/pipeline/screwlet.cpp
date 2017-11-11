#include "paint.hpp"
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
    this->fitting = geometry_freeze(this->make_fitting(fitting_rx, fitting_ry));
}

void Screwlet::fill_extent(float x, float y, float* w, float* h) {
    SET_VALUES(w, this->width, h, this->height);
}

void Screwlet::draw(CanvasDrawingSession^ ds, float x, float y, float Width, float Height) {
    float base_height = this->pipe_brush->StartPoint.x - this->pipe_thickness;
    float base_width = base_height * 2.0F + this->pipe_thickness;
    float body_y = y + base_height;
    float body_height = this->height - body_y + y;
    float body_corner = base_height * 0.5F;
    float base_y = y + this->height - base_height;
    float body_off = (base_width - this->pipe_thickness) * 0.5F;
    float base_x, body_x;

    this->locate_body(x, base_width, body_off, &base_x, &body_x);
    ds->FillRoundedRectangle(body_x, body_y, this->pipe_thickness, body_height, body_corner, body_corner, this->body_color);
    ds->FillRectangle(base_x, base_y, base_width, base_height, this->base_color);

    { // draw pipe
        float fitting_rx = this->fitting_brush->StartPoint.x;
        float fitting_off = fitting_rx * 0.1618F;
        float pipe_y = y + this->pipe_brush->StartPoint.y + this->pipe_thickness * 0.5F;
        float pipe_width = this->width - (body_off + this->pipe_thickness + this->fitting_width);
        float pipe_x, fitting_x, cx;

        brush_translate(this->fitting_brush, x, y);
        brush_translate(this->pipe_brush, x, y);
        cx = this->locate_pipe(x, body_x, fitting_rx, fitting_off, &pipe_x, &fitting_x);
        ds->FillEllipse(cx, pipe_y, fitting_rx, pipe_y - y, this->color);
        ds->DrawLine(pipe_x, pipe_y, pipe_x + pipe_width, pipe_y, this->pipe_brush, this->pipe_thickness);
        ds->DrawCachedGeometry(this->fitting, fitting_x, y, this->fitting_brush);
    }
}

/*************************************************************************************************/
LScrewlet::LScrewlet(float width, float height, float thickness, double color, double saturation, double light, double highlight)
    : Screwlet(width, height, thickness, color, saturation, light, highlight) {}
    
CanvasGeometry^ LScrewlet::make_fitting(float rx, float ry) {
    return cylinder_rl_surface(rx, ry, this->fitting_width);
}

Rect LScrewlet::get_input_port() {
    float pipe_x = this->pipe_brush->StartPoint.x;
    float pipe_length = this->width - pipe_x;
    float port_width = pipe_length * 0.618F;
    float x = pipe_x + (pipe_length - port_width) * 0.5F;

    return Rect{ x, this->pipe_brush->StartPoint.y, port_width, 0.0F };
}

Rect LScrewlet::get_output_port() {
    return Rect{ this->width, this->pipe_brush->StartPoint.y, 0.0F, this->pipe_thickness };
}

void LScrewlet::locate_body(float x, float base_width, float body_off, float *base_x, float* body_x) {
    (*base_x) = x;
    (*body_x) = x + body_off;
}

float LScrewlet::locate_pipe(float x, float body_x, float fitting_rx, float fitting_off, float *pipe_x, float* fitting_x) {
    (*fitting_x) = body_x + this->pipe_thickness - fitting_rx;
    (*pipe_x) = body_x + this->pipe_thickness + this->fitting_width - fitting_off;

    return (*pipe_x);
}

/*************************************************************************************************/
RScrewlet::RScrewlet(float width, float height, float thickness, double color, double saturation, double light, double highlight)
    : Screwlet(width, height, thickness, color, saturation, light, highlight) {}

CanvasGeometry^ RScrewlet::make_fitting(float rx, float ry) {
    return cylinder_lr_surface(rx, ry, this->fitting_width);
}

Rect RScrewlet::get_input_port() {
    float pipe_x = this->pipe_brush->StartPoint.x;
    float pipe_length = this->width - pipe_x;
    float port_width = pipe_length * 0.618F;
    float x = (this->width - port_width - pipe_x) * 0.5F; // simplified hflipping based on LScrewlet

    return Rect{ x, this->pipe_brush->StartPoint.y, port_width, 0.0F };
}

Rect RScrewlet::get_output_port() {
    return Rect{ 0.0F, this->pipe_brush->StartPoint.y, 0.0F, this->pipe_thickness };
}

void RScrewlet::locate_body(float x, float base_width, float body_off, float *base_x, float* body_x) {
    (*base_x) = x + this->width - base_width;
    (*body_x) = x + this->width - this->pipe_thickness - body_off;
}

float RScrewlet::locate_pipe(float x, float body_x, float fitting_rx, float fitting_off, float *pipe_x, float* fitting_x) {
    float cx_before_adjust = body_x - this->fitting_width;

    (*pipe_x) = x;
    (*fitting_x) = cx_before_adjust - fitting_rx;

    return cx_before_adjust + fitting_off;
}

/*
void RScrewlet::draw(CanvasDrawingSession^ ds, float x, float y, float Width, float Height) {
    { // draw pipe
        float fitting_rx = this->fitting_brush->StartPoint.x;
        float pipe_x = body_x + this->pipe_thickness;
        float pipe_y = y + this->pipe_brush->StartPoint.y + this->pipe_thickness * 0.5F;
        float fitting_off = this->fitting_width - fitting_rx * 0.1618F;
        float cx = pipe_x + fitting_off;

        brush_translate(this->fitting_brush, x, y);
        brush_translate(this->pipe_brush, x, y);
        ds->FillEllipse(cx, pipe_y, fitting_rx, pipe_y - y, this->color);
        ds->DrawLine(cx, pipe_y, x + this->width, pipe_y, this->pipe_brush, this->pipe_thickness);
        ds->DrawCachedGeometry(this->fitting, pipe_x - fitting_rx, y, this->fitting_brush);
    }
}
*/
