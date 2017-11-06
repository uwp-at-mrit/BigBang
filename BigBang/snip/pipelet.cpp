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
static const Color hat_color = hsla(194.74, 0.53, 0.79);
static const Color hat_hlcolor = hsla(40.0, 1.0, 0.97);

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
    float base_x = body_x - (base_width - this->pipe_thickness) * 0.5F;
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

        this->brush->Opacity = 0.85F;
        ds->FillRectangle(cartoon_x, cartoon_y, region.Width, region.Height, this->brush);
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

/*************************************************************************************************/
GlueCleanerlet::GlueCleanerlet(float width, float height, float thickness, double color, double saturation, double light, double highlight)
    : width(width), height(height), pipe_thickness(thickness) {
    if (thickness == 0.0F) {
        this->pipe_thickness = this->width * default_pipe_thickness_ratio;
    } else if (thickness < 0.0F) {
        this->pipe_thickness = -this->width * thickness;
    }

    this->pipe_ascent = this->pipe_thickness * 0.5F;

    this->color = hsla(color, saturation, light);
    this->highlight_color = hsla(color, saturation, highlight);
    this->body_color = hsla(color, saturation, light * 0.8);
    this->endpoint_color = hsla(color, saturation, light * 0.6);
}

void GlueCleanerlet::load() {
    Color pipe_colors[] = { this->color, this->highlight_color, this->color, this->color };
    Color hat_colors[] = { hat_color, hat_hlcolor, hat_color };
    auto pipe_stops = MAKE_GRADIENT_STOPS(pipe_colors);
    auto hat_stops = MAKE_GRADIENT_STOPS(hat_colors);

    float bottom_width = this->pipe_thickness * 1.618F;
    float base_height = (bottom_width - this->pipe_thickness) * 0.5F;
    float bottom_x = this->width - bottom_width;
    float bottom_y = this->height - base_height;
    float hat_width = this->pipe_thickness * 0.8F;
    float hat_height = base_height * 0.382F; // 0.618F x 0.618F
    float hat_x = bottom_x + (bottom_width - hat_width) * 0.5F;
    float hatbody_width = hat_width * 0.618F;
    float hatbody_height = base_height * 0.618F;
    float hatbody_x = bottom_x + (bottom_width - hatbody_width) * 0.5F;
    float top_width = bottom_width - base_height;
    float top_x = bottom_x + (bottom_width - top_width) * 0.5F;
    float top_y = hat_height + hatbody_height;
    float vpipe_x = bottom_x + (bottom_width - this->pipe_thickness) * 0.5F;
    float pipe_rx = base_height;

    auto full_pipe = rounded_rectangle(-pipe_rx, 0.0F, vpipe_x + pipe_rx * 2.0F, this->pipe_thickness, pipe_rx, -0.5F);
    auto pipe_head = rectangle(-pipe_rx, 0.0F, pipe_rx, this->pipe_thickness);
    auto top = rectangle(top_x, top_y, top_width, base_height);
    auto bottom = rectangle(bottom_x, bottom_y, bottom_width, base_height);
    this->pipe = geometry_freeze(geometry_substract(full_pipe, pipe_head));
    this->endpoint = geometry_freeze(geometry_union(top, bottom));
    this->pipe_brush = make_linear_gradient_brush(vpipe_x, top_y, vpipe_x, top_y + this->pipe_thickness, pipe_stops);
    this->hat_brush = make_linear_gradient_brush(hat_x, 0.0F, hat_x + hat_width, 0.0F, hat_stops);
    this->hatbody_brush = make_linear_gradient_brush(hatbody_x, hat_height, hatbody_x + hatbody_width, hatbody_height, hat_stops);
}

void GlueCleanerlet::fill_extent(float x, float y, float* w, float* h, float* b, float* t, float* l, float* r) {
    SET_VALUES(w, this->width, h, this->height);
    SET_BOXES(b, t, 0.0F);
    SET_BOXES(l, r, 0.0F);
}

void GlueCleanerlet::draw(CanvasDrawingSession^ ds, float x, float y, float Width, float Height) {
    float2 body = this->pipe_brush->StartPoint;
    float body_y = y + body.y;

    { // draw output pipe
        float vpipe_x = x + body.x;
        float vpipe_height = y + this->height - body_y;

        ds->FillRectangle(vpipe_x, body_y, this->pipe_thickness, vpipe_height, this->body_color);
        ds->DrawCachedGeometry(this->endpoint, x, y, this->endpoint_color);
    }

    { // draw input pipe
        brush_translate(this->pipe_brush, 0.0F, y + this->pipe_ascent);
        ds->DrawCachedGeometry(this->pipe, x, body_y + this->pipe_ascent, this->pipe_brush);
    }

    { // draw hat
        float2 hat = this->hat_brush->StartPoint;
        float2 hatbody = this->hatbody_brush->StartPoint;
        float hat_width = this->hat_brush->EndPoint.x - hat.x;
        float hatbody_width = this->hatbody_brush->EndPoint.x - hatbody.x;

        brush_translate(this->hat_brush, x, y);
        brush_translate(this->hatbody_brush, x, y);
        ds->FillRectangle(x + hat.x, y, hat_width, hatbody.y - hat.y, this->hat_brush);
        ds->FillRectangle(x + hatbody.x, y + hatbody.y, hatbody_width, body.y - hatbody.y, this->hatbody_brush);
    }
}
