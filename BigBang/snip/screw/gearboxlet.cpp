#include "paint.hpp"
#include "shape.hpp"
#include "colorspace.hpp"
#include "snip/screw/gearboxlet.hpp"
#include "snip/screw/constants.hpp"

using namespace WarGrey::SCADA;

using namespace Windows::UI;
using namespace Windows::Foundation;
using namespace Windows::Foundation::Numerics;

using namespace Microsoft::Graphics::Canvas;
using namespace Microsoft::Graphics::Canvas::Geometry;

Gearboxlet::Gearboxlet(float width, float height, float thickness, double color, double saturation, double light, double highlight)
    : width(width), height(height), pipe_thickness(thickness) {
    if (thickness <= 0.0F) {
        this->pipe_thickness = this->width * default_pipe_thickness_ratio;
    } else if (thickness < 0.0F) {
        this->pipe_thickness = -this->width * thickness;
    }

    this->fitting_width = this->pipe_thickness * default_fitting_width_pipe_ratio;

    this->color = hsla(color, saturation, light);
    this->highlight_color = hsla(color, saturation, highlight);
    this->fitting_color = hsla(color, saturation, light * default_fitting_lightness_rate);
    this->body_color = hsla(color, saturation, light * default_body_lightness_rate);
    this->gbox_color = hsla(color, saturation, light * default_endpoint_lightness_rate);
}

void Gearboxlet::construct() {
    Color fitting_colors[] = { this->fitting_color, this->highlight_color, this->fitting_color, this->fitting_color };
    Color pipe_colors[] = { this->color, this->highlight_color, this->color, this->color };
    auto fitting_stops = MAKE_GRADIENT_STOPS(fitting_colors);
    auto pipe_stops = MAKE_GRADIENT_STOPS(pipe_colors);

    float ascent = this->pipe_thickness * 0.5F;
    float basefit_ry = this->pipe_thickness * 0.5F + ascent;
    float basefit_rx = basefit_ry * default_fitting_view_angle;
    float outfit_ry = this->pipe_thickness * default_fitting_height_pipe_ratio * 0.5F;
    float outfit_rx = outfit_ry * default_fitting_view_angle;
    float outfit_y = basefit_ry - outfit_ry;
    float base_width = this->pipe_thickness * 1.618F;
	float base_height = this->height - basefit_ry * 2.0F - ascent;
	float motor_x = (this->width - this->pipe_thickness) * 0.382F;
    float pipe_part_x = motor_x + this->pipe_thickness;
    
    this->basefit_brush = make_linear_gradient_brush(basefit_rx, base_height, basefit_rx, base_height + basefit_ry * 2.0F, fitting_stops);
    this->outfit_brush = make_linear_gradient_brush(outfit_rx, outfit_y, outfit_rx, outfit_y + outfit_ry * 2.0F, fitting_stops);
    this->pipe_brush = make_linear_gradient_brush(pipe_part_x, ascent, pipe_part_x, this->pipe_thickness + ascent, pipe_stops);
    this->base_fitting = geometry_freeze(this->make_fitting(basefit_rx, basefit_ry));
    this->outlet_fitting = geometry_freeze(this->make_fitting(outfit_rx, outfit_ry));
}

void Gearboxlet::fill_extent(float x, float y, float* w, float* h) {
    SET_VALUES(w, this->width, h, this->height);
}

void Gearboxlet::draw(CanvasDrawingSession^ ds, float x, float y, float Width, float Height) {
    float body_x, pipe_x, base_x, basefit_x, basefit_cx, outfit_x, outfit_cx;
    
    float motor_x = this->pipe_brush->StartPoint.x - this->pipe_thickness;
    float tray_width = motor_x + this->pipe_thickness + this->fitting_width * 2.0F;
	float tray_height = this->fitting_width;
	float base_height = this->basefit_brush->StartPoint.y;
	float base_width = tray_width - this->fitting_width * 2.0F;
	float base_xoff = (tray_width - base_width) * 0.5F;
	float body_y = y + this->fitting_width;
    float body_height = this->height - base_height - body_y + y + this->fitting_width;
    float body_corner = this->pipe_thickness * 0.125F;
    float base_y = y + this->height - base_height;
    
    this->locate_body(x, base_width, motor_x, &body_x, &base_x);
    ds->FillRoundedRectangle(body_x, body_y, this->pipe_thickness, body_height, body_corner, body_corner, this->body_color);
    ds->FillRectangle(base_x, base_y, tray_width, tray_height, this->gbox_color);
	ds->FillRectangle(base_x + base_xoff, base_y + tray_height, base_width, base_height, this->body_color);

    { // draw pipe
        float basefit_rx = this->basefit_brush->StartPoint.x;
        float outfit_rx = this->outfit_brush->StartPoint.x;
        float outfit_ry = (this->outfit_brush->EndPoint.y - this->outfit_brush->StartPoint.y) * 0.5F;
        float pipe_length = this->width - (motor_x + this->pipe_thickness + this->fitting_width * 2.0F + outfit_rx);
        float cy = y + this->pipe_brush->StartPoint.y + this->pipe_thickness * 0.5F;
        
        brush_translate(this->pipe_brush, x, y);
        brush_translate(this->basefit_brush, x, y - base_height);
        brush_translate(this->outfit_brush, x, y);
        this->locate_pipe(x, body_x, 0.1618F, basefit_rx, outfit_rx, &pipe_x, &basefit_x, &basefit_cx, &outfit_x, &outfit_cx);
        ds->FillEllipse(basefit_cx, cy, basefit_rx, cy - y, this->color);
        ds->DrawLine(pipe_x, cy, pipe_x + pipe_length, cy, this->pipe_brush, this->pipe_thickness);
        ds->DrawCachedGeometry(this->base_fitting, basefit_x, y, this->basefit_brush);
        ds->FillEllipse(outfit_cx, cy, outfit_rx, outfit_ry, this->color);
        ds->DrawCachedGeometry(this->outlet_fitting, outfit_x, y + this->outfit_brush->StartPoint.y, this->outfit_brush);
    }
}

/*************************************************************************************************/
LGearboxlet::LGearboxlet(float width, float height, float thickness, double color, double saturation, double light, double highlight)
    : Gearboxlet(width, height, thickness, color, saturation, light, highlight) {}
    
CanvasGeometry^ LGearboxlet::make_fitting(float rx, float ry) {
    return cylinder_rl_surface(rx, ry, this->fitting_width);
}

Rect LGearboxlet::get_input_port() {
    float pipe_part_x = this->pipe_brush->StartPoint.x;
    float pipe_part_length = this->width - pipe_part_x;
    float socket_width = pipe_part_length * 0.618F;
    float x = pipe_part_x + (pipe_part_length - socket_width) * 0.5F;

    return Rect{ x, this->pipe_brush->StartPoint.y, socket_width, 0.0F };
}

Rect LGearboxlet::get_output_port() {
    float socket_width = this->outfit_brush->StartPoint.x;

    return Rect{ this->width - socket_width, this->pipe_brush->StartPoint.y, socket_width, this->pipe_thickness };
}

Rect LGearboxlet::get_motor_port() {
	float base_y = this->pipe_brush->StartPoint.y;
	float motor_x = this->pipe_brush->StartPoint.x - this->pipe_thickness;
	float base_height = this->basefit_brush->StartPoint.y;

	return Rect{ motor_x, base_y, 0.0F, this->height - base_y - base_height };
}

void LGearboxlet::locate_body(float x, float base_width, float body_off, float* body_x, float *base_x) {
    (*base_x) = x;
    (*body_x) = x + body_off;
}

void LGearboxlet::locate_pipe(float x, float body_x, float offrate, float basefit_rx, float outfit_rx
    ,float* pipe_x, float* basefit_x, float *basefit_cx, float* outfit_x, float* outfit_cx) {
    (*basefit_x) = body_x + this->pipe_thickness - basefit_rx;
    (*basefit_cx) = body_x + this->pipe_thickness + this->fitting_width - basefit_rx * offrate;
    (*outfit_cx) = x + this->width - outfit_rx;
    (*outfit_x) = (*outfit_cx) - this->fitting_width - outfit_rx + outfit_rx * offrate;
    (*pipe_x) = (*basefit_cx);
}

/*************************************************************************************************/
RGearboxlet::RGearboxlet(float width, float height, float thickness, double color, double saturation, double light, double highlight)
    : Gearboxlet(width, height, thickness, color, saturation, light, highlight) {}

CanvasGeometry^ RGearboxlet::make_fitting(float rx, float ry) {
    return cylinder_lr_surface(rx, ry, this->fitting_width);
}

Rect RGearboxlet::get_input_port() {
    float pipe_part_x = this->pipe_brush->StartPoint.x;
    float pipe_part_length = this->width - pipe_part_x;
    float socket_width = pipe_part_length * 0.618F;
    float x = (this->width - socket_width - pipe_part_x) * 0.5F; // simplified hflipping based on LGearboxlet

    return Rect{ x, this->pipe_brush->StartPoint.y, socket_width, 0.0F };
}

Rect RGearboxlet::get_output_port() {
    float socket_width = this->outfit_brush->StartPoint.x;

    return Rect{ 0.0F, this->pipe_brush->StartPoint.y, socket_width, this->pipe_thickness };
}

Rect RGearboxlet::get_motor_port() {
	float base_y = this->pipe_brush->StartPoint.y;
	float motor_x = this->pipe_brush->StartPoint.x - this->pipe_thickness;
	float base_height = this->basefit_brush->StartPoint.y;

	return Rect{ this->width - motor_x, base_y, 0.0F, this->height - base_y - base_height };
}

void RGearboxlet::locate_body(float x, float base_width, float body_off, float* body_x, float *base_x) {
    (*base_x) = x + this->width - base_width;
    (*body_x) = x + this->width - this->pipe_thickness - body_off;
}

void RGearboxlet::locate_pipe(float x, float body_x, float offrate, float basefit_rx, float outfit_rx
    , float* pipe_x, float* basefit_x, float *basefit_cx, float* outfit_x, float* outfit_cx) {
    float basecx_before_adjust = body_x - this->fitting_width;
    
    (*basefit_x) = basecx_before_adjust - basefit_rx;
    (*basefit_cx) = basecx_before_adjust + basefit_rx * offrate;
    (*outfit_x) = x - outfit_rx * offrate;
    (*outfit_cx) = x + outfit_rx;
    (*pipe_x) = (*outfit_x) + this->fitting_width + outfit_rx;
}
