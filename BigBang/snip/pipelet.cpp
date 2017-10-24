#include <cmath>

#include "text.hpp"
#include "paint.hpp"
#include "geometry.hpp"
#include "colorspace.hpp"
#include "snip/pipelet.hpp"

using namespace WarGrey::SCADA;

using namespace Windows::UI;
using namespace Microsoft::Graphics::Canvas;

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
