#include "paint.hpp"
#include "shape.hpp"
#include "colorspace.hpp"
#include "snip/screw/gluecleanerlet.hpp"
#include "snip/screw/constants.hpp"

using namespace WarGrey::SCADA;

using namespace Windows::UI;
using namespace Windows::Foundation;
using namespace Windows::Foundation::Numerics;

using namespace Microsoft::Graphics::Canvas;
using namespace Microsoft::Graphics::Canvas::Geometry;

static const Color hat_color = hsla(194.74, 0.53, 0.79);
static const Color hat_hlcolor = hsla(40.0, 1.0, 0.97);

GlueCleanerlet::GlueCleanerlet(float width, float height, float thickness, double color, double saturation, double light, double highlight)
    : width(width), height(height), pipe_thickness(thickness) {
    if (thickness == 0.0F) {
        this->pipe_thickness = this->width * default_pipe_thickness_ratio;
    } else if (thickness < 0.0F) {
        this->pipe_thickness = -this->width * thickness;
    }

    this->fitting_width = this->pipe_thickness * default_fitting_width_pipe_ratio;

    this->color = hsla(color, saturation, light);
    this->highlight_color = hsla(color, saturation, highlight);
    this->fitting_color = hsla(color, saturation, light * default_fitting_lightness_rate);
    this->body_color = hsla(color, saturation, light * default_body_lightness_rate);
    this->endpoint_color = hsla(color, saturation, light * default_endpoint_lightness_rate);
}

void GlueCleanerlet::load() {
    Color fitting_colors[] = { this->fitting_color, this->highlight_color, this->fitting_color, this->fitting_color };
    Color pipe_colors[] = { this->color, this->highlight_color, this->color, this->color };
    Color hat_colors[] = { hat_color, hat_hlcolor, hat_color };
    auto fitting_stops = MAKE_GRADIENT_STOPS(fitting_colors); 
    auto pipe_stops = MAKE_GRADIENT_STOPS(pipe_colors);
    auto hat_stops = MAKE_GRADIENT_STOPS(hat_colors);

    float bottom_width = this->pipe_thickness * 1.618F;
    float base_height = (bottom_width - this->pipe_thickness) * 0.5F;
    float bottom_x = this->width - bottom_width;
    float bottom_y = this->height - base_height;
    float hat_width = this->pipe_thickness * 0.8F;
    float hat_x = bottom_x + (bottom_width - hat_width) * 0.5F;
    float hatbody_width = hat_width * 0.618F;
    float hatbody_height = base_height * 0.618F;
    float hatbody_x = bottom_x + (bottom_width - hatbody_width) * 0.5F;
    float hatbody_y = base_height * 0.382F; // 0.618F x 0.618F
    float top_width = bottom_width - base_height;
    float top_x = bottom_x + (bottom_width - top_width) * 0.5F;
    float top_y = hatbody_y + hatbody_height;
    float vpipe_x = bottom_x + (bottom_width - this->pipe_thickness) * 0.5F;
    float hpipe_y = top_y + this->pipe_thickness * 0.5F;
    float fitting_height = this->pipe_thickness * default_fitting_height_pipe_ratio;
    float fitting_ry = fitting_height * 0.5F;
    float fitting_rx = fitting_ry * default_fitting_view_angle;
    float fitting_y = hpipe_y - (fitting_height - this->pipe_thickness) * 0.5F;
    float pipe_rx = base_height;

    auto top = rectangle(top_x, top_y, top_width, base_height);
    auto bottom = rectangle(bottom_x, bottom_y, bottom_width, base_height);
    auto full_pipe = rounded_rectangle(0.0F, 0.0F, vpipe_x + pipe_rx, this->pipe_thickness, pipe_rx, -0.5F);
    auto pipe_head = rectangle(0.0F, 0.0F, this->fitting_width + fitting_rx, this->pipe_thickness);
    this->pipe = geometry_freeze(geometry_substract(full_pipe, pipe_head));
    this->fitting = geometry_freeze(cylinder_rl_surface(fitting_rx, fitting_ry, this->fitting_width));
    this->endpoint = geometry_freeze(geometry_union(top, bottom));
    this->pipe_brush = make_linear_gradient_brush(vpipe_x, hpipe_y, vpipe_x, hpipe_y + this->pipe_thickness, pipe_stops);
    this->hat_brush = make_linear_gradient_brush(hat_x, hatbody_y, hat_x + hat_width, hatbody_y, hat_stops);
    this->hatbody_brush = make_linear_gradient_brush(hatbody_x, top_y, hatbody_x + hatbody_width, top_y, hat_stops);
    this->fitting_brush = make_linear_gradient_brush(fitting_rx, fitting_y, fitting_rx, fitting_y + fitting_height, fitting_stops);
}

void GlueCleanerlet::fill_extent(float x, float y, float* w, float* h) {
    SET_VALUES(w, this->width, h, this->height);
}

Rect GlueCleanerlet::get_input_port() {
    return Rect{ 0.0F, this->pipe_brush->StartPoint.y, this->fitting_brush->StartPoint.x, this->pipe_thickness };
}

Rect GlueCleanerlet::get_output_port() {
    return Rect{ this->pipe_brush->StartPoint.x, this->height, this->pipe_thickness, 0.0F };
}

Rect GlueCleanerlet::get_motor_port() {
	float hat_x = this->hat_brush->StartPoint.x;
	float hat_width = this->hat_brush->EndPoint.x - hat_x;
	
	return Rect{ hat_x, 0.0, hat_width, 0.0F };
}

void GlueCleanerlet::draw(CanvasDrawingSession^ ds, float x, float y, float Width, float Height) {
    float body_y = y + this->hatbody_brush->StartPoint.y;

    { // draw output pipe
        float vpipe_x = x + this->pipe_brush->StartPoint.x;
        float vpipe_height = y + this->height - body_y;

        ds->FillRectangle(vpipe_x, body_y, this->pipe_thickness, vpipe_height, this->body_color);
        ds->DrawCachedGeometry(this->endpoint, x, y, this->endpoint_color);
    }

    { // draw input pipe
        float hpipe_y = y + this->pipe_brush->StartPoint.y;
        float fitting_y = y + this->fitting_brush->StartPoint.y;
        float fitting_rx = this->fitting_brush->StartPoint.x;
        float fitting_cx = x + this->fitting_width + fitting_rx * (1.0F - default_fitting_view_angle);
        float fitting_cy = hpipe_y + this->pipe_thickness * 0.5F;

        brush_translate(this->fitting_brush, x, y);
        brush_translate(this->pipe_brush, x, y);
        ds->FillEllipse(fitting_cx, fitting_cy, fitting_rx, fitting_cy - fitting_y, this->color);
        ds->DrawCachedGeometry(this->pipe, x, hpipe_y, this->pipe_brush);
        ds->DrawCachedGeometry(this->fitting, x, fitting_y, this->fitting_brush);
    }

    { // draw hat
        float2 hat_bottom = this->hat_brush->StartPoint;
        float2 hatbody_bottom = this->hatbody_brush->StartPoint;
        float hat_width = this->hat_brush->EndPoint.x - hat_bottom.x;
        float hatbody_width = this->hatbody_brush->EndPoint.x - hatbody_bottom.x;
        float hatbody_y = y + hat_bottom.y;

        brush_translate(this->hat_brush, x, y);
        brush_translate(this->hatbody_brush, x, y);
        ds->FillRectangle(x + hat_bottom.x, y, hat_width, hat_bottom.y, this->hat_brush);
        ds->FillRectangle(x + hatbody_bottom.x, hatbody_y, hatbody_width, body_y - hatbody_y, this->hatbody_brush);
    }
}
