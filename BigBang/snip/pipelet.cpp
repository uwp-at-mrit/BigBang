#include "rsyslog.hpp"
#include "text.hpp"
#include "paint.hpp"
#include "shape.hpp"
#include "colorspace.hpp"
#include "snip/pipelet.hpp"

using namespace WarGrey::SCADA;

using namespace Windows::UI;
using namespace Microsoft::Graphics::Canvas;
using namespace Windows::Foundation::Numerics;

static const float default_ratio = 0.2F;

/*************************************************************************************************/
Screwlet::Screwlet(float width, float height, float thickness, double color, double saturation, double body, double light)
    : width(width), height(height), pipe_thickness(thickness) {
    if (thickness == 0.0F) {
        this->pipe_thickness = this->width * default_ratio;
    } else if (thickness < 0.0F) {
        this->pipe_thickness = -this->width * thickness;
    }

    this->pipe_ascent = this->pipe_thickness * 0.5F;
    this->connector_width = this->width * 0.0618F;
    this->connector_rx = this->pipe_ascent * 0.1618F;

    this->connector_color = hsla(color, saturation, body * 0.5);
    this->color = hsla(color, saturation, body);
    this->highlight_color = hsla(color, saturation, light); 
}

void Screwlet::load() {
    Color connector_colors[] = { this->connector_color, this->highlight_color, this->connector_color, this->connector_color };
    Color pipe_colors[] = { this->color, this->highlight_color, this->color, this->color };
    float connector_height = this->pipe_thickness + this->pipe_ascent * 2.0F;
    
    this->connector_brush = make_linear_gradient_brush(0.0F, connector_height, MAKE_GRADIENT_STOPS(connector_colors));
    this->pipe_brush = make_linear_gradient_brush(0.0F, this->pipe_thickness, MAKE_GRADIENT_STOPS(pipe_colors));
    this->connector = geometry_freeze(cylinder_rl_surface(this->connector_rx, connector_height * 0.5F, this->connector_width));
}

void Screwlet::fill_extent(float x, float y, float* w, float* h, float* b, float* t, float* l, float* r) {
    SET_VALUES(w, this->width, h, this->height);
    SET_BOXES(b, t, 0.0F);
    SET_BOXES(l, r, 0.0F);
}

void Screwlet::update(long long count, long long interval, long long uptime, bool is_slow) {
    //this->engine->update(count, interval, uptime, is_slow);
}

void Screwlet::draw(CanvasDrawingSession^ ds, float x, float y, float Width, float Height) {
    float connector_height = this->connector_brush->EndPoint.y - this->connector_brush->StartPoint.y;
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
        float pipe_y = y + connector_height * 0.5F;
        float pipe_xoff = this->connector_width - this->connector_rx * 0.5F;

        brush_translate(this->connector_brush, x, y);
        brush_translate(this->pipe_brush, 0.0F, pipe_y - this->pipe_thickness * 0.5F);
        ds->FillEllipse(pipe_x + pipe_xoff, pipe_y, this->connector_rx, pipe_y - y, this->color);
        ds->DrawLine(pipe_x, pipe_y, x + this->width, pipe_y, this->pipe_brush, this->pipe_thickness);
        ds->DrawCachedGeometry(this->connector, pipe_x - this->connector_rx, y, this->connector_brush);
    }
}

/*************************************************************************************************/
HPipelet::HPipelet(float width, float height, double color) : width(width), height(height) {
    this->color = hsla(color, 0.7, 0.3);
    this->highlight_color = hsla(color, 0.7, 0.8);
}

void HPipelet::load() {
    Color colors[] = { this->color, this->color, this->highlight_color, this->color, this->color, this->color };

    this->brush = make_linear_gradient_brush(0.0F, this->height, MAKE_GRADIENT_STOPS(colors));
}

void HPipelet::fill_extent(float x, float y, float* w, float* h, float* b, float* t, float* l, float* r) {
    SET_VALUES(w, this->width, h, this->height);
    SET_BOXES(b, t, 0.0F);
    SET_BOXES(l, r, 0.0F);
}

void HPipelet::update(long long count, long long interval, long long uptime, bool is_slow) {

}

void HPipelet::draw(CanvasDrawingSession^ ds, float x, float y, float Width, float Height) {
    auto pipe = rounded_rectangle(0.0F, 0.0F, this->width, this->height, this->height * 0.30F, -0.5F);
    
    brush_translate(this->brush, x, y);
    ds->FillGeometry(pipe, x, y, this->brush);
}
