#include "paint.hpp"
#include "system.hpp"
#include "transformation.hpp"

using namespace Windows::UI;
using namespace Windows::UI::ViewManagement;

using namespace Windows::Foundation::Numerics;

using namespace Microsoft::Graphics::Canvas;
using namespace Microsoft::Graphics::Canvas::Brushes;
using namespace Microsoft::Graphics::Canvas::Geometry;

static CanvasDevice^ shared_ds = CanvasDevice::GetSharedDevice();

GradientStops^ make_gradient_stops(Color colors[], int total) {
    GradientStops^ stopa = nullptr;
    CanvasGradientStop* stops = new CanvasGradientStop[total];
    auto flstep = 1.0F / float(total - 1);

    for (int i = 0; i < total; i++) {
        stops[i] = CanvasGradientStop{ float(i) * flstep, colors[i] };
    }

    stopa = ref new Platform::Array<CanvasGradientStop>(stops, total);
    delete[] stops;

    return stopa;
}

void brush_translate(ICanvasBrush^ brush, float x, float y) {
    brush->Transform = make_translation_matrix(x, y);
}

CanvasSolidColorBrush^ make_solid_brush(Color& color) {
    return ref new CanvasSolidColorBrush(shared_ds, color);
}

CanvasLinearGradientBrush^ make_linear_gradient_brush(float sx, float sy, float ex, float ey
    , GradientStops^ stops, CanvasEdgeBehavior edge, CanvasAlphaMode alpha) {
    auto brush = ref new CanvasLinearGradientBrush(shared_ds, stops, edge, alpha);

    brush->StartPoint = float2(sx, sy);
    brush->EndPoint = float2(ex, ey);

    return brush;
}

CanvasLinearGradientBrush^ make_linear_gradient_brush(float hextent, float vextent, GradientStops^ stops
    , CanvasEdgeBehavior edge, CanvasAlphaMode alpha) {
    return make_linear_gradient_brush(0.0F, 0.0F, hextent, vextent, stops, edge, alpha);
}

CanvasStrokeStyle^ make_dash_stroke(Platform::Array<float>^ dashes, float offset) {
    auto dash = ref new CanvasStrokeStyle();

    dash->DashOffset = offset;
    dash->CustomDashStyle = dashes;

    return dash;
}

CanvasStrokeStyle^ make_dash_stroke(CanvasDashStyle style, float offset) {
    auto dash = ref new CanvasStrokeStyle();
    dash->DashOffset = offset;
    dash->DashStyle = style;

    return dash;
}

/*************************************************************************************************/
#define implement_solid_brush(name, Color) \
CanvasSolidColorBrush^ ##name##_brush() { \
    static CanvasSolidColorBrush^ name = make_solid_brush(Color); \
    return name; \
}

#define implement_system_brush(name, UIEVAL) \
CanvasSolidColorBrush^ system_##name##_brush() { \
    static CanvasSolidColorBrush^ name = make_solid_brush(system_color(UIEVAL)); \
    return name; \
}

implement_solid_brush(transparent, Colors::Transparent)

implement_system_brush(accentdark1, UIColorType::AccentDark1)
implement_system_brush(background, UIElementType::Background)
implement_system_brush(foreground, UIElementType::HighlightText)
implement_system_brush(highlight, UIElementType::Highlight)
implement_system_brush(graytext, UIElementType::GrayText)

