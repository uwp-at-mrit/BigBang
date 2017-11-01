#include <cmath>

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
Screwlet::Screwlet(float width, float height, float thickness) : width(width), height(height), thickness(thickness) {
    if (thickness == 0.0F) {
        this->thickness = this->width * default_ratio;
    } else if (thickness < 0.0F) {
        this->thickness = -this->width * thickness;
    }
}

void Screwlet::load() {
    //{
    //    this->engine->info = this->info; // TODO: is it safe?
    //    this->engine->load();
    //}
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
    //float engine_width, engine_height;
    //this->engine->fill_extent(x, y, &engine_width, &engine_height);

    { // draw body
        float base_height = 8.0F;
        float body_x = x + base_height;
        float base_x = body_x - base_height;
        float base_y = y + this->height - base_height;
        float base_width = this->thickness + base_height * 2.0F;

        ds->FillRoundedRectangle(body_x, y, this->thickness, this->height, 4.0F, 4.0F, Colors::ForestGreen);
        ds->FillRectangle(base_x, base_y, base_width, base_height, Colors::ForestGreen);
        ds->DrawRectangle(base_x, base_y, base_width, base_height, Colors::Black);

        { // draw pipe
            float pipe_x = body_x + this->thickness;
            float pipe_y = y + base_height;
            float pipe_width = x + this->width - pipe_x;
            float pipe_height = this->thickness;
            float endpoint_height = pipe_height / 0.618F;

            ds->FillRectangle(pipe_x, pipe_y, pipe_width, pipe_height, Colors::ForestGreen);
            ds->FillEllipse(pipe_x, pipe_y + pipe_height * 0.5F, 3.0F, endpoint_height * 0.5F, Colors::ForestGreen);
        }
    }

    { // draw engine
      //ds->Transform = make_float3x2_scale(float2(-1.0F, 1.0F));
      //this->engine->draw(ds, -(x + engine_width), y + (this->height - engine_height), Width, Height);
      //this->engine->draw(ds, x, y + (this->height - engine_height), Width, Height);
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
