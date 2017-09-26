#define _USE_MATH_DEFINES
#include <cmath>

#include "text.hpp"
#include "tongue.hpp"
#include "gradient.hpp"
#include "snip/storagelet.hpp"

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

#define AUTO_LINEAR_GRADIENT_BRUSH(brush, info, x, y, width, colors) \
    auto brush = make_linear_gradient_brush( \
       info, x, y, x + width, y, \
       colors, sizeof(colors) / sizeof(Windows::UI::Color))

static Color topface_colors[] = {
    Colors::Gold, Colors::Gold, Colors::Gold, Colors::Gold,
    Colors::Gold, Colors::LightGray, Colors::Gold, Colors::Gold
};

static Color body_colors[] = {
    Colors::Black, Colors::Black, Colors::Silver, Colors::Black,
    Colors::Black, Colors::Black, Colors::Black, Colors::Black
};

static Color used_indicator_colors[] = {
    Colors::DimGray, Colors::DimGray, Colors::Silver, Colors::DimGray,
    Colors::DimGray, Colors::DimGray, Colors::DimGray, Colors::DimGray
};

/*************************************************************************************************/
StorageTanklet::StorageTanklet(float width, float height) {
    this->width = width;
    this->height = height;

    this->label_font = make_text_format(12.0F);
    this->label_font->WordWrapping = CanvasWordWrapping::WholeWord;
    this->label_font->HorizontalAlignment = CanvasHorizontalAlignment::Center;
}

void StorageTanklet::fill_extent(float x, float y, float* w, float* h, float* b, float* t, float* l, float* r) {
    SET_VALUES(w, this->width, h, this->height);
    SET_BOXES(b, t, 0.0F);
    SET_BOXES(l, r, 0.0F);
}

void StorageTanklet::draw(CanvasDrawingSession^ ds, float x, float y, float Width, float Height) {
    auto label = ref new CanvasTextLayout(ds, speak("storagetank"), this->label_font, 0.0F, 0.0F);
    auto radiusX = this->width / 2.0F;
    auto radiusY = this->width / 32.0F;
    auto body_height = this->height - radiusY * 2.0F;
    auto used_indicator_height = body_height * 0.618F;
    auto adjust_height = std::fmin(body_height - used_indicator_height + 1.0F, body_height);

    AUTO_CYLINDER_PATHBUILDER(body_tank, ds, x, y, radiusX, radiusY, this->width, adjust_height);
    AUTO_LINEAR_GRADIENT_BRUSH(body_brush, this->info, x, y, this->width, body_colors);
    ds->FillGeometry(CanvasGeometry::CreatePath(body_tank), body_brush);

    // drawing top face after drawing body makes the edge more smoothing.
    AUTO_LINEAR_GRADIENT_BRUSH(topface_brush, this->info, x, y, this->width, topface_colors);
    ds->FillEllipse(x + radiusX, y + radiusY, radiusX, radiusY, topface_brush);

    auto used_indicator_y = y + (body_height - used_indicator_height);
    AUTO_CYLINDER_PATHBUILDER(used_indicator_tank, ds, x, used_indicator_y, radiusX, radiusY, this->width, used_indicator_height);
    AUTO_LINEAR_GRADIENT_BRUSH(used_indicator_brush, this->info, x, used_indicator_y, this->width, used_indicator_colors);
    ds->FillGeometry(CanvasGeometry::CreatePath(used_indicator_tank), used_indicator_brush);

    // TextLayout has already moved the text half-width ahead since its width is zero.
    float label_x = this->width / 2.0F;
    float label_y = (this->height - label->LayoutBounds.Height) / 2.0F;

    ds->DrawTextLayout(label, x + label_x, y + label_y, Colors::White);
}
