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

    this->connector_width = this->width * 0.0618F;

    this->color = hsla(color, saturation, body);
    this->highlight_color = hsla(color, saturation, highlight);
    this->connector_color = hsla(color, saturation, body * 0.5);
}

void Screwlet::load() {
    Color connector_colors[] = { this->connector_color, this->highlight_color, this->connector_color, this->connector_color };
    Color pipe_colors[] = { this->color, this->highlight_color, this->color, this->color };
    auto connector_stops = MAKE_GRADIENT_STOPS(connector_colors);
    auto pipe_stops = MAKE_GRADIENT_STOPS(pipe_colors);

    float ascent = this->pipe_thickness * 0.5F;
    float connector_rx = ascent * 0.1618F;
    float connector_ry = this->pipe_thickness * 0.5F + ascent;
    float base_width = this->pipe_thickness * 1.618F;
    float base_height = (base_width - this->pipe_thickness) * 0.5F;
    float pipe_x = base_height + this->pipe_thickness;
    
    this->connector_brush = make_linear_gradient_brush(connector_rx, 0.0F, connector_rx, connector_ry * 2.0F, connector_stops);
    this->pipe_brush = make_linear_gradient_brush(pipe_x, ascent, pipe_x, this->pipe_thickness + ascent, pipe_stops);
    this->connector = geometry_freeze(cylinder_rl_surface(connector_rx, connector_ry, this->connector_width));
}

void Screwlet::fill_extent(float x, float y, float* w, float* h, float* b, float* t, float* l, float* r) {
    SET_VALUES(w, this->width, h, this->height);
    SET_BOXES(b, t, 0.0F);
    SET_BOXES(l, r, 0.0F);
}

void Screwlet::fill_inport_extent(float* x, float* y, float* width, float* height) {
    float pipe_x = this->pipe_brush->StartPoint.x;
    float pipe_length = this->width - pipe_x;
    float input_width = pipe_length * 0.618F;

    SET_BOX(x, pipe_x + (pipe_length - input_width) * 0.5F);
    SET_BOX(y, this->pipe_brush->StartPoint.y);
    SET_BOX(width, input_width);
    SET_BOX(height, this->pipe_thickness * default_pipe_thickness_ratio);
}

void Screwlet::fill_outport_extent(float* x, float* y, float* width, float* height) {
    SET_BOX(x, this->width);
    SET_BOX(y, this->pipe_brush->StartPoint.y);
    SET_BOX(width, 0.0F);
    SET_BOX(height, this->pipe_thickness);
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

    ds->FillRoundedRectangle(body_x, body_y, this->pipe_thickness, body_height, body_corner, body_corner, this->color);
    ds->FillRectangle(base_x, base_y, base_width, base_height, this->color);
    ds->DrawLine(body_x, base_y, body_x + this->pipe_thickness, base_y, Colors::Black);

    { // draw pipe
        float connector_rx = this->connector_brush->StartPoint.x;
        float pipe_x = body_x + this->pipe_thickness;
        float pipe_y = y + this->pipe_brush->StartPoint.y + this->pipe_thickness * 0.5F;
        float pipe_xoff = this->connector_width - connector_rx * 0.5F;

        brush_translate(this->connector_brush, x, y);
        brush_translate(this->pipe_brush, x, y);
        ds->FillEllipse(pipe_x + pipe_xoff, pipe_y, connector_rx, pipe_y - y, this->color);
        ds->DrawLine(pipe_x + pipe_xoff, pipe_y, x + this->width, pipe_y, this->pipe_brush, this->pipe_thickness);
        ds->DrawCachedGeometry(this->connector, pipe_x - connector_rx, y, this->connector_brush);
    }

    IPipelet::draw(ds, x, y, Width, Height);
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

    this->connector_width = this->width * 0.0618F;

    this->color = hsla(color, saturation, body);
    this->highlight_color = hsla(color, saturation, highlight);
    this->connector_color = hsla(color, saturation, body * 0.5);
}

void Pipelet::load() {
    Color connector_colors[] = { this->connector_color, this->highlight_color, this->connector_color, this->connector_color };
    Color colors[] = { this->color, this->highlight_color, this->color, this->color };
    auto connector_stops = MAKE_GRADIENT_STOPS(connector_colors);
    auto pipe_stops = MAKE_GRADIENT_STOPS(colors);

    float ascent = (this->height - this->thickness) * 0.5F;
    float connector_rx = ascent * 0.18F;

    float pipe_xoff = this->connector_width + connector_rx * 0.5F;
    float pipe_width = this->width - pipe_xoff;
    float hollow_height = this->thickness * 0.618F;
    float hollow_offset = (this->thickness - hollow_height) * 0.5F;
    float hollow_width = pipe_width - hollow_offset - hollow_offset;

    this->connector_brush = make_linear_gradient_brush(connector_rx, 0.0F, connector_rx, this->height, connector_stops);
    this->brush = make_linear_gradient_brush(pipe_xoff, ascent, pipe_xoff, ascent + this->thickness, pipe_stops);
    this->connector = geometry_freeze(cylinder_rl_surface(connector_rx, this->height * 0.5F, this->connector_width));

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

void Pipelet::fill_inport_extent(float* x, float* y, float* width, float* height) {
    SET_BOX(x, this->connector_brush->StartPoint.x);
    SET_BOX(y, this->brush->StartPoint.y);
    SET_BOX(width, 0.0F);
    SET_BOX(height, this->thickness);
}

void Pipelet::fill_outport_extent(float* x, float* y, float* width, float* height) {
    SET_BOX(x, this->width);
    SET_BOX(y, this->brush->StartPoint.y);
    SET_BOX(width, 0.0F);
    SET_BOX(height, this->thickness);
}

void Pipelet::update(long long count, long long interval, long long uptime, bool is_slow) {
    this->cartoon_style->DashOffset = -float(count);
}

void Pipelet::draw(CanvasDrawingSession^ ds, float x, float y, float Width, float Height) {
    float pipe_x = x + this->brush->StartPoint.x;
    float pipe_y = y + this->brush->StartPoint.y;
    float cy = pipe_y + this->thickness * 0.5F;
    
    brush_translate(this->brush, x, y);
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
        ds->FillEllipse(pipe_x, cy, this->connector_brush->StartPoint.x, cy - y, this->color);
        ds->DrawCachedGeometry(this->hollow_body, pipe_x, pipe_y, this->brush);
        ds->DrawCachedGeometry(this->connector, x, y, this->connector_brush);
    }

    IPipelet::draw(ds, x, y, Width, Height);
}

/*************************************************************************************************/
GlueCleanerlet::GlueCleanerlet(float width, float height, float thickness, double color, double saturation, double light, double highlight)
    : width(width), height(height), pipe_thickness(thickness) {
    if (thickness == 0.0F) {
        this->pipe_thickness = this->width * default_pipe_thickness_ratio;
    } else if (thickness < 0.0F) {
        this->pipe_thickness = -this->width * thickness;
    }

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
    float pipe_rx = base_height;

    auto full_pipe = rounded_rectangle(-pipe_rx, 0.0F, vpipe_x + pipe_rx * 2.0F, this->pipe_thickness, pipe_rx, -0.5F);
    auto pipe_head = rectangle(-pipe_rx, 0.0F, pipe_rx, this->pipe_thickness);
    auto top = rectangle(top_x, top_y, top_width, base_height);
    auto bottom = rectangle(bottom_x, bottom_y, bottom_width, base_height);
    this->pipe = geometry_freeze(geometry_substract(full_pipe, pipe_head));
    this->endpoint = geometry_freeze(geometry_union(top, bottom));
    this->pipe_brush = make_linear_gradient_brush(vpipe_x, hpipe_y, vpipe_x, hpipe_y + this->pipe_thickness, pipe_stops);
    this->hat_brush = make_linear_gradient_brush(hat_x, hatbody_y, hat_x + hat_width, hatbody_y, hat_stops);
    this->hatbody_brush = make_linear_gradient_brush(hatbody_x, top_y, hatbody_x + hatbody_width, top_y, hat_stops);
}

void GlueCleanerlet::fill_extent(float x, float y, float* w, float* h, float* b, float* t, float* l, float* r) {
    SET_VALUES(w, this->width, h, this->height);
    SET_BOXES(b, t, 0.0F);
    SET_BOXES(l, r, 0.0F);
}

void GlueCleanerlet::fill_inport_extent(float* x, float* y, float* width, float* height) {
    SET_BOX(x, 0.0F);
    SET_BOX(y, this->pipe_brush->StartPoint.y);
    SET_BOX(width, 0.0F);
    SET_BOX(height, this->pipe_thickness);
}

void GlueCleanerlet::fill_outport_extent(float* x, float* y, float* width, float* height) {
    SET_BOX(x, this->pipe_brush->StartPoint.x);
    SET_BOX(y, this->height);
    SET_BOX(width, this->pipe_thickness);
    SET_BOX(height, 0.0F);
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
        brush_translate(this->pipe_brush, 0.0F, y);
        ds->DrawCachedGeometry(this->pipe, x, y + this->pipe_brush->StartPoint.y, this->pipe_brush);
    }

    { // draw hat_bottom
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

    IPipelet::draw(ds, x, y, Width, Height);
}
