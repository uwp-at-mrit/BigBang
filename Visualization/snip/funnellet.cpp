#include "path.hpp"
#include "gradient.hpp"
#include "snip/funnellet.hpp"

using namespace WarGrey::SCADA;

using namespace Windows::UI;
using namespace Microsoft::Graphics::Canvas;
using namespace Microsoft::Graphics::Canvas::Brushes;

static Color topface_colors[] = { Colors::DarkGoldenrod, Colors::DarkGoldenrod, Colors::LightGray, Colors::DarkGoldenrod };
static Color body_colors[] = { Colors::DarkGoldenrod, Colors::LightGray, Colors::DarkGoldenrod };

static Platform::Array<CanvasGradientStop>^ topface_stops = nullptr;
static Platform::Array<CanvasGradientStop>^ body_stops = nullptr;

/*************************************************************************************************/
Funnellet::Funnellet(float width, float height) {
    this->width = width;
    this->height = height;

    if (body_stops == nullptr) {
        topface_stops = MAKE_GRADIENT_STOPS(topface_colors);
        body_stops = MAKE_GRADIENT_STOPS(body_colors);
    }
}

void Funnellet::fill_extent(float x, float y, float* w, float* h, float* b, float* t, float* l, float* r) {
    SET_VALUES(w, this->width, h, this->height);
    SET_BOXES(b, t, 0.0F);
    SET_BOXES(l, r, 0.0F);
}

void Funnellet::draw(CanvasDrawingSession^ ds, float x, float y, float Width, float Height) {
    auto radiusT = this->width / 2.0F;
    auto radiusB = this->width / 8.0F;
    auto radiusY = this->width / 64.0F;
    auto body_height = this->height - radiusY * 3.0F;

    auto body_path = make_pyramid_surface(ds, x, y, radiusT, radiusB, radiusY, body_height);
    auto body_brush = make_linear_gradient_brush(this->info, x, y, x + this->width, y, body_stops);
    ds->FillGeometry(body_path, body_brush);

    // drawing top face after drawing body makes the edge more smoothing.
    auto topface_brush = make_linear_gradient_brush(this->info, x, y, x + this->width, y, topface_stops);
    ds->FillEllipse(x + radiusT, y + radiusY, radiusT, radiusY, topface_brush);
}
