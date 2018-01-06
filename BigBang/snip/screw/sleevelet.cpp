#include "snip/screw/sleevelet.hpp"
#include "snip/screw/constants.hpp"
#include "colorspace.hpp"
#include "paint.hpp"
#include "shape.hpp"

using namespace WarGrey::SCADA;

using namespace Windows::UI;
using namespace Windows::Foundation;
using namespace Windows::Foundation::Numerics;

using namespace Microsoft::Graphics::Canvas;
using namespace Microsoft::Graphics::Canvas::Geometry;

Sleevelet::Sleevelet(float width, float height, float thickness, double color, double saturation, double light, double highlight)
    : width(width), height(height), thickness(thickness) {
    if (thickness <= 0.0F) {
        this->thickness = this->width * default_pipe_thickness_ratio;
    } else if (thickness < 0.0F) {
        this->thickness = -this->width * thickness;
    }

    if (height <= 0.0F) {
        this->height = this->thickness * default_fitting_height_pipe_ratio;
    }

    this->fitting_width = this->thickness * default_fitting_width_pipe_ratio;

    this->color = hsla(color, saturation, light);
    this->highlight_color = hsla(color, saturation, highlight);
    this->fitting_color = hsla(color, saturation, light * default_fitting_lightness_rate);
}

void Sleevelet::load() {
    Color colors[] = { this->color, this->highlight_color, this->color, this->color };
    Color fitting_colors[] = { this->fitting_color, this->highlight_color, this->fitting_color, this->fitting_color };
    auto fitting_stops = MAKE_GRADIENT_STOPS(fitting_colors);
    
    float ascent = (this->height - this->thickness) * 0.5F;
    float fitting_ry = this->height * 0.5F;
    float fitting_rx = fitting_ry * default_fitting_view_angle;
    float pipe_length = this->width - (this->fitting_width + fitting_rx) * 2.0F;
    float pipe_x = (this->width - pipe_length) * 0.5F;
    float hollow_height = this->thickness * 0.618F;
    float hollow_offset = (this->thickness - hollow_height) * 0.5F;
    float hollow_width = pipe_length - hollow_offset * 2.0F;
    float hollow_x = (this->width - hollow_width) * 0.5F;

    this->brush = make_linear_gradient_brush(pipe_x, ascent, pipe_x, ascent + this->thickness, MAKE_GRADIENT_STOPS(colors));
    this->fitting_brush = make_linear_gradient_brush(fitting_rx, 0.0F, fitting_rx, this->height, fitting_stops);
    this->fitting = geometry_freeze(this->make_fitting(fitting_rx, fitting_ry));
    
    auto pipe = rectangle(pipe_x, ascent, pipe_length, this->thickness);
    this->cartoon_style = make_dash_stroke(CanvasDashStyle::Dash);
    this->body_mask = rectangle(hollow_x, ascent + hollow_offset, hollow_width, hollow_height);
    this->hollow_body = geometry_freeze(geometry_substract(pipe, this->body_mask));
}

void Sleevelet::fill_extent(float x, float y, float* w, float* h) {
    SET_VALUES(w, this->width, h, this->height);
}

void Sleevelet::draw(CanvasDrawingSession^ ds, float x, float y, float Width, float Height) {
    brush_translate(this->brush, x, y);
    brush_translate(this->fitting_brush, x, y);

    { // visualize inner state
        Rect region = this->body_mask->ComputeBounds();
        float pipe_x = x + this->brush->StartPoint.x;
        float cartoon_x = x + region.Left;
        float cartoon_xend = cartoon_x + region.Width;
        float cartoon_y = y + region.Top;
        float cartoon_ytop = cartoon_y + 1.0F;
        float cartoon_ybottom = cartoon_ytop + region.Height - 2.0F;

        this->brush->Opacity = 0.72F;
        ds->FillRectangle(cartoon_x, cartoon_y, region.Width, region.Height, this->brush);
        ds->DrawLine(cartoon_x, cartoon_ytop, cartoon_xend, cartoon_ytop, this->highlight_color, 2.0F, this->cartoon_style);
        ds->DrawLine(pipe_x, cartoon_ybottom, cartoon_xend, cartoon_ybottom, this->highlight_color, 2.0F, this->cartoon_style);
    }

    { // draw body
        float fitting_rx = this->fitting_brush->StartPoint.x;
        float fitting_ry = this->height * 0.5F;
        float fitting_off = fitting_rx * 0.1618F;
        float infit_x, outfit_x, infit_cx, outfit_cx;
        
        this->brush->Opacity = 1.0F;
        this->locate_pipe(x, fitting_rx, fitting_off, &infit_x, &infit_cx, &outfit_x, &outfit_cx);
        ds->FillEllipse(infit_cx, y + fitting_ry, fitting_rx, fitting_ry, this->color);
        ds->FillEllipse(outfit_cx, y + fitting_ry, fitting_rx, fitting_ry, this->color);
        ds->DrawCachedGeometry(this->hollow_body, x, y, this->brush);
        ds->DrawCachedGeometry(this->fitting, infit_x, y, this->fitting_brush);
        ds->DrawCachedGeometry(this->fitting, outfit_x, y, this->fitting_brush);
    }
}

/*************************************************************************************************/
LSleevelet::LSleevelet(float width, float height, float thickness, double color, double saturation, double light, double highlight)
    : Sleevelet(width, height, thickness, color, saturation, light, highlight) {}

void LSleevelet::update(long long count, long long interval, long long uptime, bool is_slow) {
    this->cartoon_style->DashOffset = -float(count);
}

Rect LSleevelet::get_input_port() {
    float socket_width = this->fitting_brush->StartPoint.x;

    return Rect{ 0.0F, this->brush->StartPoint.y, socket_width, this->thickness };
}

Rect LSleevelet::get_output_port() {
    float socket_width = this->fitting_brush->StartPoint.x;

    return Rect{ this->width - socket_width, this->brush->StartPoint.y, socket_width, this->thickness };
}

CanvasGeometry^ LSleevelet::make_fitting(float rx, float ry) {
    return cylinder_rl_surface(rx, ry, this->fitting_width);
}

void LSleevelet::locate_pipe(float x, float rx, float off, float* infit_x, float *infit_cx, float* outfit_x, float* outfit_cx) {
    (*infit_cx) = x + this->fitting_width + rx - off;
    (*infit_x) = x;
    (*outfit_cx) = x + this->width - rx;
    (*outfit_x) = (*outfit_cx) - this->fitting_width - rx + off;
}

/*************************************************************************************************/
RSleevelet::RSleevelet(float width, float height, float thickness, double color, double saturation, double light, double highlight)
    : Sleevelet(width, height, thickness, color, saturation, light, highlight) {}

void RSleevelet::update(long long count, long long interval, long long uptime, bool is_slow) {
    this->cartoon_style->DashOffset = float(count);
}

Rect RSleevelet::get_input_port() {
    float socket_width = this->fitting_brush->StartPoint.x;

    return Rect{ this->width - socket_width, this->brush->StartPoint.y, socket_width, this->thickness };
}

Rect RSleevelet::get_output_port() {
    float socket_width = this->fitting_brush->StartPoint.x;

    return Rect{ 0.0F, this->brush->StartPoint.y, socket_width, this->thickness };
}

CanvasGeometry^ RSleevelet::make_fitting(float rx, float ry) {
    return cylinder_lr_surface(rx, ry, this->fitting_width);
}

void RSleevelet::locate_pipe(float x, float rx, float off, float* infit_x, float *infit_cx, float* outfit_x, float* outfit_cx) {
    (*infit_x) = x + this->width - this->fitting_width - rx * 2.0F;
    (*infit_cx) = (*infit_x) + rx + off; 
    (*outfit_x) = x - off;
    (*outfit_cx) = x + rx;
}
