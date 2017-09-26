#define _USE_MATH_DEFINES
#include <cmath>

#include "text.hpp"
#include "tongue.hpp"
#include "gradient.hpp"
#include "snip/funnellet.hpp"

using namespace WarGrey::SCADA;

using namespace Windows::UI;
using namespace Microsoft::Graphics::Canvas;
using namespace Microsoft::Graphics::Canvas::Text;
using namespace Microsoft::Graphics::Canvas::Brushes;
using namespace Microsoft::Graphics::Canvas::Geometry;

using namespace Windows::Foundation::Numerics;

#define AUTO_CYLINDER_PATHBUILDER(tank, ds, x, y, rx, ry, w, h) \
    auto tank = ref new CanvasPathBuilder(ds); \
    tank->BeginFigure(x, y); \
    tank->AddArc(float2(x + rx, y + ry), rx, ry, float(M_PI), -float(M_PI)); \
    tank->AddLine(x + w, y + h); \
    tank->AddArc(float2(x + rx, y + h + ry), rx, ry, 0.0F, float(M_PI)); \
    tank->EndFigure(CanvasFigureLoop::Closed)

#define AUTO_LINEAR_GRADIENT_BRUSH(brush, info, sx, sy, ex, ey, stops) \
    auto brush = ref new CanvasLinearGradientBrush(info, stops, CanvasEdgeBehavior::Mirror, CanvasAlphaMode::Premultiplied); \
    brush->StartPoint = float2(sx, sy); \
    brush->EndPoint = float2(ex, ey)

static Platform::Array<CanvasGradientStop>^ topface_stops = nullptr;
static Platform::Array<CanvasGradientStop>^ body_stops = nullptr;
static Platform::Array<CanvasGradientStop>^ used_stops = nullptr;

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

    this->label_font = make_text_format(12.0F);
    this->label_font->WordWrapping = CanvasWordWrapping::WholeWord;
    this->label_font->HorizontalAlignment = CanvasHorizontalAlignment::Center;

    if (body_stops == nullptr) {
        topface_stops = make_color_stops(Colors::Gold, Colors::LightGray, 0.7F);
        body_stops = make_color_stops(Colors::Black, Colors::Silver, 0.3F);
        used_stops = make_color_stops(Colors::DimGray, Colors::Silver, 0.3F);
    }
}

void Funnellet::fill_extent(float x, float y, float* w, float* h, float* b, float* t, float* l, float* r) {
    SET_VALUES(w, this->width, h, this->height);
    SET_BOXES(b, t, 0.0F);
    SET_BOXES(l, r, 0.0F);
}

void Funnellet::draw(CanvasDrawingSession^ ds, float x, float y, float Width, float Height) {
    auto label = ref new CanvasTextLayout(ds, speak("storagetank"), this->label_font, 0.0F, 0.0F);
    auto radiusX = this->width / 2.0F;
    auto radiusY = this->width / 32.0F;
    auto body_height = this->height - radiusY * 2.0F;
    auto used_height = body_height * 0.618F;
    auto adjust_height = std::fmin(body_height - used_height + 1.0F, body_height);

    Color body_colors[] = {
        Colors::Black, Colors::Black, Colors::Black,
        Colors::Silver,
        Colors::Black, Colors::Black, Colors::Black, Colors::Black
    };

    Color used_colors[] = {
        Colors::DimGray, Colors::DimGray, Colors::DimGray,
        Colors::Silver,
        Colors::DimGray, Colors::DimGray, Colors::DimGray, Colors::DimGray
    };

    AUTO_CYLINDER_PATHBUILDER(body_tank, ds, x, y, radiusX, radiusY, this->width, adjust_height);
    auto body_brush = make_linear_gradient_brush(this->info, -100.0, y, 100.0, y, body_colors, 8);
    ds->FillGeometry(CanvasGeometry::CreatePath(body_tank), body_brush);

    // drawing top face after drawing body makes the edge more smoothing.
    AUTO_LINEAR_GRADIENT_BRUSH(topface_brush, this->info, x, y, x + this->width, y, topface_stops);
    ds->FillEllipse(x + radiusX, y + radiusY, radiusX, radiusY, topface_brush);

    auto used_y = y + (body_height - used_height);
    AUTO_CYLINDER_PATHBUILDER(used_tank, ds, x, used_y, radiusX, radiusY, this->width, used_height);
    auto used_brush = make_linear_gradient_brush(this->info, -100.0, used_y, 100.0, used_y, used_colors, 8);
    ds->FillGeometry(CanvasGeometry::CreatePath(used_tank), used_brush);

    // TextLayout has already moved the text half-width ahead since its width is zero.
    float label_x = this->width / 2.0F;
    float label_y = (this->height - label->LayoutBounds.Height) / 2.0F;

    ds->DrawTextLayout(label, x + label_x, y + label_y, Colors::White);
}
