#include "rsyslog.hpp"
#include "text.hpp"
#include "paint.hpp"
#include "shape.hpp"
#include "colorspace.hpp"
#include "snip/pipelet.hpp"

using namespace WarGrey::SCADA;

using namespace Windows::UI;
using namespace Windows::Foundation;
using namespace Windows::Foundation::Numerics;

using namespace Microsoft::Graphics::Canvas;
using namespace Microsoft::Graphics::Canvas::Geometry;

static const float default_pipe_thickness_ratio = 0.2F;

/*************************************************************************************************/
Screwlet::Screwlet(float width, float height, float thickness, double color, double saturation, double body, double highlight)
    : width(width), height(height), pipe_thickness(thickness) {
    if (thickness == 0.0F) {
        this->pipe_thickness = this->width * default_pipe_thickness_ratio;
    } else if (thickness < 0.0F) {
        this->pipe_thickness = -this->width * thickness;
    }

    this->pipe_ascent = this->pipe_thickness * 0.5F;
    this->connector_width = this->width * 0.0618F;
    this->connector_rx = this->pipe_ascent * 0.1618F;

    this->color = hsla(color, saturation, body);
    this->highlight_color = hsla(color, saturation, highlight);
    this->connector_color = hsla(color, saturation, body * 0.5);
}

void Screwlet::load() {
    Color connector_colors[] = { this->connector_color, this->highlight_color, this->connector_color, this->connector_color };
    Color pipe_colors[] = { this->color, this->highlight_color, this->color, this->color };
    float cnctor_ry = this->pipe_thickness * 0.5F + this->pipe_ascent;
    
    this->connector_brush = make_linear_gradient_brush(0.0F, cnctor_ry + cnctor_ry, MAKE_GRADIENT_STOPS(connector_colors));
    this->pipe_brush = make_linear_gradient_brush(0.0F, this->pipe_thickness, MAKE_GRADIENT_STOPS(pipe_colors));
    this->connector = geometry_freeze(cylinder_rl_surface(this->connector_rx, cnctor_ry, this->connector_width));
}

void Screwlet::fill_extent(float x, float y, float* w, float* h, float* b, float* t, float* l, float* r) {
    SET_VALUES(w, this->width, h, this->height);
    SET_BOXES(b, t, 0.0F);
    SET_BOXES(l, r, 0.0F);
}

void Screwlet::draw(CanvasDrawingSession^ ds, float x, float y, float Width, float Height) {
    float base_width = this->pipe_thickness * 1.618F;
    float base_height = (base_width - this->pipe_thickness) * 0.5F;
    float body_x = x + base_height;
    float body_y = y + base_height;
    float body_height = this->height - body_y + y;
    float body_corner = base_height * 0.5F;
    float base_x = body_x - base_height;
    float base_y = y + this->height - base_height;

    ds->FillRoundedRectangle(body_x, body_y, this->pipe_thickness, body_height, body_corner, body_corner, this->color);
    ds->FillRectangle(base_x, base_y, base_width, base_height, this->color);
    ds->DrawRectangle(base_x, base_y, base_width, base_height, Colors::Black);

    { // draw pipe
        float pipe_x = body_x + this->pipe_thickness;
        float pipe_y = y + this->pipe_ascent + this->pipe_thickness * 0.5F;
        float pipe_xoff = this->connector_width - this->connector_rx * 0.5F;

        brush_translate(this->connector_brush, x, y);
        brush_translate(this->pipe_brush, 0.0F, y + this->pipe_ascent);
        ds->FillEllipse(pipe_x + pipe_xoff, pipe_y, this->connector_rx, pipe_y - y, this->color);
        ds->DrawLine(pipe_x + pipe_xoff, pipe_y, x + this->width, pipe_y, this->pipe_brush, this->pipe_thickness);
        ds->DrawCachedGeometry(this->connector, pipe_x - this->connector_rx, y, this->connector_brush);
    }
}

/*************************************************************************************************/
Pipelet::Pipelet(float width, float height, float thickness, double color, double saturation, double body, double highlight)
    : width(width), height(height), thickness(thickness) {
    if (thickness == 0.0F) {
        this->thickness = this->width * default_pipe_thickness_ratio;
    } else if (thickness < 0.0F) {
        this->thickness = -this->width * thickness;
    }

    if (height <= 0.0F) {
        this->height = this->thickness * 1.618F;
    }

    this->ascent = (this->height - this->thickness) * 0.5F;
    this->connector_width = this->width * 0.0618F;
    this->connector_rx = this->ascent * 0.18F;

    this->color = hsla(color, saturation, body);
    this->highlight_color = hsla(color, saturation, highlight);
    this->connector_color = hsla(color, saturation, body * 0.5);
}

void Pipelet::load() {
    Color connector_colors[] = { this->connector_color, this->highlight_color, this->connector_color, this->connector_color };
    Color colors[] = { this->color, this->highlight_color, this->color, this->color };
    float pipe_xoff = this->connector_width + this->connector_rx * 0.5F;
    float pipe_width = this->width - pipe_xoff;
    float hollow_height = this->thickness * 0.618F;
    float hollow_offset = (this->thickness - hollow_height) * 0.5F;
    float hollow_width = pipe_width - hollow_offset - hollow_offset;

    this->connector_brush = make_linear_gradient_brush(0.0F, this->height, MAKE_GRADIENT_STOPS(connector_colors));
    this->brush = make_linear_gradient_brush(pipe_xoff, 0.0F, pipe_xoff, this->thickness, MAKE_GRADIENT_STOPS(colors));
    this->connector = geometry_freeze(cylinder_rl_surface(this->connector_rx, this->height * 0.5F, this->connector_width));

    auto pipe = rectangle(pipe_width, this->thickness);
    this->body_mask = rectangle(hollow_offset, hollow_offset, hollow_width, hollow_height);
    //auto cartoon_top = hline(hollow_offset, hollow_offset + 1.0F, hollow_width);
    //auto cartoon_bottom = hline(hollow_offset, this->thickness - hollow_offset - 1.0F, hollow_width);
    this->hollow_body = geometry_freeze(geometry_substract(pipe, this->body_mask));
    this->cartoon_style = make_dash_stroke(CanvasDashStyle::Dash);
}

void Pipelet::fill_extent(float x, float y, float* w, float* h, float* b, float* t, float* l, float* r) {
    SET_VALUES(w, this->width, h, this->height);
    SET_BOXES(b, t, 0.0F);
    SET_BOXES(l, r, 0.0F);
}

void Pipelet::update(long long count, long long interval, long long uptime, bool is_slow) {
    this->cartoon_style->DashOffset = -float(count);
}

void Pipelet::draw(CanvasDrawingSession^ ds, float x, float y, float Width, float Height) {
    float pipe_x = x + this->brush->StartPoint.x;
    float pipe_y = y + this->ascent;
    float cy = pipe_y + this->thickness * 0.5F;
    
    brush_translate(this->brush, x, pipe_y);
    brush_translate(this->connector_brush, x, y);

    { // visualize inner state
        Rect region = this->body_mask->ComputeBounds();
        float cartoon_x = pipe_x + region.Left;
        float cartoon_xend = cartoon_x + region.Width;
        float cartoon_y = pipe_y + region.Top;
        float cartoon_ytop = cartoon_y + 1.0F;
        float cartoon_ybottom = cartoon_ytop + region.Height - 2.0F;

        this->brush->Opacity = 0.72F;
        ds->FillRectangle(cartoon_x, pipe_y + region.Top, region.Width, region.Height, this->brush);
        ds->DrawLine(cartoon_x, cartoon_ytop, cartoon_xend, cartoon_ytop, this->highlight_color, 2.0F, this->cartoon_style);
        ds->DrawLine(x, cartoon_ybottom, cartoon_xend, cartoon_ybottom, this->highlight_color, 2.0F, this->cartoon_style);
    }

    { // draw pipe
        this->brush->Opacity = 1.0F;
        ds->FillEllipse(pipe_x, cy, this->connector_rx, cy - y, this->color);
        ds->DrawCachedGeometry(this->hollow_body, pipe_x, pipe_y, this->brush);
        ds->DrawCachedGeometry(this->connector, x, y, this->connector_brush);
    }
}
