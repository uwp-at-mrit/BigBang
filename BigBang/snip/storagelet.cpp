#include <cmath>

#include "text.hpp"
#include "tongue.hpp"
#include "paint.hpp"
#include "geometry.hpp"
#include "snip/storagelet.hpp"

using namespace WarGrey::SCADA;

using namespace Windows::UI;
using namespace Microsoft::Graphics::Canvas;
using namespace Microsoft::Graphics::Canvas::Text;
using namespace Microsoft::Graphics::Canvas::Brushes;

static const float default_ratio = 1.618F;

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

static Platform::Array<CanvasGradientStop>^ topface_stops = nullptr;
static Platform::Array<CanvasGradientStop>^ body_stops = nullptr;
static Platform::Array<CanvasGradientStop>^ used_stops = nullptr;

/*************************************************************************************************/
StorageTanklet::StorageTanklet(float width, float height) : width(width), height(height) {
    if (height == 0.0F) {
        this->height = width * default_ratio;
    } else if (height < 0.0F) {
        this->height = -(width * height);
    }

    this->label_font = make_text_format(12.0F, CanvasWordWrapping::WholeWord, CanvasHorizontalAlignment::Center);

    if (body_stops == nullptr) {
        topface_stops = MAKE_GRADIENT_STOPS(topface_colors);
        body_stops = MAKE_GRADIENT_STOPS(body_colors);
        used_stops = MAKE_GRADIENT_STOPS(used_colors);
    }
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
    auto used_height = body_height * 0.618F;
    auto adjust_height = std::fmin(body_height - used_height + 1.0F, body_height);

    auto body_brush = make_linear_gradient_brush(x, y, x + this->width, y, body_stops);
    auto body_path = cylinder_surface(x, y, radiusX, radiusY, adjust_height);
    ds->FillGeometry(body_path, body_brush);

    // drawing top face after drawing body makes the edge more smoothing.
    auto topface_brush = make_linear_gradient_brush(x, y, x + this->width, y, topface_stops);
    ds->FillEllipse(x + radiusX, y + radiusY, radiusX, radiusY, topface_brush);

    auto used_y = y + (body_height - used_height);
    auto used_indicator_brush = make_linear_gradient_brush(x, used_y, x + this->width, used_y, used_stops);
    auto used_path = cylinder_surface(x, used_y, radiusX, radiusY, used_height);
    ds->FillGeometry(used_path, used_indicator_brush);

    // TextLayout has already moved the text half-width ahead since its width is zero.
    float label_x = this->width / 2.0F;
    float label_y = (this->height - label->LayoutBounds.Height) / 2.0F;

    ds->DrawTextLayout(label, x + label_x, y + label_y, Colors::White);
}
