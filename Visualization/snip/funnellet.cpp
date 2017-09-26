#include "text.hpp"
#include "tongue.hpp"
#include "gradient.hpp"
#include "snip/funnellet.hpp"

using namespace WarGrey::SCADA;

using namespace Windows::UI;
using namespace Microsoft::Graphics::Canvas;
using namespace Microsoft::Graphics::Canvas::Text;
using namespace Microsoft::Graphics::Canvas::Brushes;

static Color topface_colors[] = {
    Colors::Gold, Colors::Gold, Colors::Gold, Colors::Gold,
    Colors::Gold, Colors::LightGray, Colors::Gold, Colors::Gold
};

static Color body_colors[] = {
    Colors::Black, Colors::Black, Colors::Silver, Colors::Black,
    Colors::Black, Colors::Black, Colors::Black, Colors::Black
};

static Color used_colors[] = {
    Colors::DimGray, Colors::DimGray, Colors::Silver, Colors::DimGray,
    Colors::DimGray, Colors::DimGray, Colors::DimGray, Colors::DimGray
};

static Platform::Array<CanvasGradientStop>^ make_color_stops(Color& edge_color, Color& highlight_color, float position) {
    CanvasGradientStop stops[] = {
        CanvasGradientStop{ 0.0F, edge_color },
        CanvasGradientStop{ position, highlight_color },
        CanvasGradientStop{ 1.0F, edge_color }
    };

    return ref new Platform::Array<CanvasGradientStop>(stops, sizeof(stops) / sizeof(CanvasGradientStop));
}

/*************************************************************************************************/
Funnellet::Funnellet(float width, float height) {
    this->width = width;
    this->height = height;
}

void Funnellet::fill_extent(float x, float y, float* w, float* h, float* b, float* t, float* l, float* r) {
    SET_VALUES(w, this->width, h, this->height);
    SET_BOXES(b, t, 0.0F);
    SET_BOXES(l, r, 0.0F);
}

void Funnellet::draw(CanvasDrawingSession^ ds, float x, float y, float Width, float Height) {
    auto radiusX = this->width / 2.0F;
    auto radiusB = radiusX / 4.0F;
    auto radiusY = this->width / 64.0F;
    auto body_height = this->height - radiusY * 2.0F;

    //AUTO_FUNNEL_PATHBUILDER(body_tank, ds, x, y, radiusX, radiusY, this->width, this->height);
    //auto body_brush = make_linear_gradient_brush(this->info, -100.0, y, 100.0, y, body_colors, 8);
    //ds->FillGeometry(CanvasGeometry::CreatePath(body_tank), body_brush);

    // drawing top face after drawing body makes the edge more smoothing.
    // AUTO_LINEAR_GRADIENT_BRUSH(topface_brush, this->info, x, y, x + this->width, y, topface_stops);
    // ds->FillEllipse(x + radiusX, y + radiusY, radiusX, radiusY, topface_brush);
}
