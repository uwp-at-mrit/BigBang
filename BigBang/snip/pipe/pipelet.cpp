#include "paint.hpp"
#include "shape.hpp"
#include "colorspace.hpp"
#include "snip/pipe/pipelet.hpp"
#include "snip/pipe/static.hpp"

using namespace WarGrey::SCADA;

using namespace Windows::UI;
using namespace Windows::Foundation;
using namespace Windows::Foundation::Numerics;

using namespace Microsoft::Graphics::Canvas;
using namespace Microsoft::Graphics::Canvas::Geometry;

Pipelet::Pipelet(float width, float height, float thickness, double color, double saturation, double light, double highlight)
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

    this->color = hsla(color, saturation, light);
    this->highlight_color = hsla(color, saturation, highlight);
    this->connector_color = hsla(color, saturation, light * default_connector_light_rate);
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

Rect Pipelet::get_input_port() {
    return Rect{ this->connector_brush->StartPoint.x, this->brush->StartPoint.y, 0.0F, this->thickness };
}

Rect Pipelet::get_output_port() {
    return Rect{ this->width, this->brush->StartPoint.y, 0.0F, this->thickness };
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

        this->brush->Opacity = 0.72F;
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
}
