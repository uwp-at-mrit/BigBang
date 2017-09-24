#include <cmath>

#include "text.hpp"
#include "tongue.hpp"
#include "snip/storagelet.hpp"

using namespace WarGrey::SCADA;

using namespace Windows::UI;
using namespace Microsoft::Graphics::Canvas;
using namespace Microsoft::Graphics::Canvas::Text;
using namespace Microsoft::Graphics::Canvas::Brushes;

using namespace Windows::Foundation::Numerics;

#define AUTO_LINEAR_GRADIENT_BRUSH(brush, info, sx, sy, ex, ey, stops) \
    auto brush = ref new CanvasLinearGradientBrush(info, stops); \
    brush->StartPoint = float2(sx, sy); \
    brush->EndPoint = float2(ex, ey);

Platform::Array<CanvasGradientStop>^ topface_stops = nullptr;
Platform::Array<CanvasGradientStop>^ body_stops = nullptr;
Platform::Array<CanvasGradientStop>^ used_stops = nullptr;

static Platform::Array<CanvasGradientStop>^ make_color_stops(Color& edge_color, Color& highlight_color, float position) {
    auto stops = ref new Platform::Array<CanvasGradientStop>(3);
    stops->get(0).Color = edge_color; 
    stops->get(0).Position = 0.0F;
    stops->get(1).Color = highlight_color;
    stops->get(1).Position = position;
    stops->get(2).Color = edge_color;
    stops->get(2).Position = 1.0F;
    return stops;
}

StorageTanklet::StorageTanklet(float width, float height) {
    this->width = width;
    this->height = height;

    this->label_font = make_text_format(12.0F);

    if (body_stops == nullptr) {
        topface_stops = make_color_stops(Colors::Purple, Colors::Gray, 0.7F);
        body_stops = make_color_stops(Colors::Black, Colors::White, 0.3F);
        used_stops = make_color_stops(Colors::DarkGray, Colors::White, 0.3F);
    }
}

void StorageTanklet::fill_extent(float x, float y, float* w, float* h, float* b, float* t, float* l, float* r) {
    SET_VALUES(w, this->width, h, this->height);
    SET_BOXES(b, t, 0.0F);
    SET_BOXES(l, r, 0.0F);
}

void StorageTanklet::draw(CanvasDrawingSession^ ds, float x, float y, float Width, float Height) {
    auto part = this->height / 3.0F;

    AUTO_LINEAR_GRADIENT_BRUSH(topface_brush, this->info, x, y, x + this->width, y, topface_stops);
    ds->FillRectangle(x, y, this->width, part, topface_brush);

    y += part;
    AUTO_LINEAR_GRADIENT_BRUSH(body_brush, this->info, x, y, x + this->width, y, body_stops);
    ds->FillRectangle(x, y, this->width, part, body_brush);
    
    y += part;
    AUTO_LINEAR_GRADIENT_BRUSH(used_brush, this->info, x, y, x + this->width, y, used_stops);
    ds->FillRectangle(x, y, this->width, part, used_brush);
}
