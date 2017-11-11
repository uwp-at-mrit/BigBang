#include "paint.hpp"
#include "shape.hpp"
#include "colorspace.hpp"
#include "snip/pipeline/pipelet.hpp"
#include "snip/pipeline/static.hpp"

using namespace WarGrey::SCADA;

using namespace Windows::UI;
using namespace Windows::Foundation;
using namespace Windows::Foundation::Numerics;

using namespace Microsoft::Graphics::Canvas;
using namespace Microsoft::Graphics::Canvas::Geometry;

Pipelet::Pipelet(float width, float thickness, double color, double saturation, double light, double highlight)
    : width(width), thickness(thickness) {
    if (thickness == 0.0F) {
        this->thickness = this->width * default_pipe_thickness_ratio;
    } else if (thickness < 0.0F) {
        this->thickness = -this->width * thickness;
    }

    this->color = hsla(color, saturation, light);
    this->highlight_color = hsla(color, saturation, highlight);
}

void Pipelet::load() {
    Color colors[] = { this->color, this->highlight_color, this->color, this->color };
    
    float hollow_height = this->thickness * 0.618F;
    float hollow_offset = (this->thickness - hollow_height) * 0.5F;
    float hollow_width = this->width - hollow_offset - hollow_offset;

    this->brush = make_linear_gradient_brush(0.0F, this->thickness, MAKE_GRADIENT_STOPS(colors));
    
    auto pipe = rectangle(this->width, this->thickness);
    this->body_mask = rectangle(hollow_offset, hollow_offset, hollow_width, hollow_height);
    this->hollow_body = geometry_freeze(geometry_substract(pipe, this->body_mask));
    this->cartoon_style = make_dash_stroke(CanvasDashStyle::Dash);
}

void Pipelet::fill_extent(float x, float y, float* w, float* h) {
    SET_VALUES(w, this->width, h, this->thickness);
}

Rect Pipelet::get_input_port() {
    return Rect{ 0.0F, 0.0F, 0.0F, this->thickness };
}

Rect Pipelet::get_output_port() {
    return Rect{ this->width, 0.0F, 0.0F, this->thickness };
}

void Pipelet::update(long long count, long long interval, long long uptime, bool is_slow) {
    this->cartoon_style->DashOffset = -float(count);
}

void Pipelet::draw(CanvasDrawingSession^ ds, float x, float y, float Width, float Height) {
    brush_translate(this->brush, x, y);

    { // visualize inner state
        Rect region = this->body_mask->ComputeBounds();
        float cartoon_x = x + region.Left;
        float cartoon_xend = cartoon_x + region.Width;
        float cartoon_y = y + region.Top;
        float cartoon_ytop = cartoon_y + 1.0F;
        float cartoon_ybottom = cartoon_ytop + region.Height - 2.0F;

        this->brush->Opacity = 0.72F;
        ds->FillRectangle(cartoon_x, cartoon_y, region.Width, region.Height, this->brush);
        ds->DrawLine(cartoon_x, cartoon_ytop, cartoon_xend, cartoon_ytop, this->highlight_color, 2.0F, this->cartoon_style);
        ds->DrawLine(x, cartoon_ybottom, cartoon_xend, cartoon_ybottom, this->highlight_color, 2.0F, this->cartoon_style);
    }

    this->brush->Opacity = 1.0F;
    ds->DrawCachedGeometry(this->hollow_body, x, y, this->brush);
}
