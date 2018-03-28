#include "paint.hpp"
#include "system.hpp"
#include "colorspace.hpp"
#include "transformation.hpp"

using namespace Windows::UI;
using namespace Windows::UI::ViewManagement;

using namespace Windows::Foundation::Numerics;

using namespace Microsoft::Graphics::Canvas;
using namespace Microsoft::Graphics::Canvas::Brushes;
using namespace Microsoft::Graphics::Canvas::Geometry;

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
    return ref new CanvasSolidColorBrush(CanvasDevice::GetSharedDevice(), color);
}

CanvasSolidColorBrush^ make_solid_brush(unsigned int hex, double alpha) {
	return ref new CanvasSolidColorBrush(CanvasDevice::GetSharedDevice(), rgba(hex, alpha));
}

CanvasLinearGradientBrush^ make_linear_gradient_brush(float sx, float sy, float ex, float ey
    , GradientStops^ stops, CanvasEdgeBehavior edge, CanvasAlphaMode alpha) {
    auto brush = ref new CanvasLinearGradientBrush(CanvasDevice::GetSharedDevice(), stops, edge, alpha);

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

CanvasSolidColorBrush^ green_brush() {
	static CanvasSolidColorBrush^ real_green = make_solid_brush(0x00FF00);

	return real_green;
}

implement_system_brush(accentdark1, UIColorType::AccentDark1)
implement_system_brush(background, UIElementType::Background)
implement_system_brush(foreground, UIElementType::HighlightText)
implement_system_brush(highlight, UIElementType::Highlight)
implement_system_brush(graytext, UIElementType::GrayText)

implement_solid_brush(transparent, Colors::Transparent)
implement_solid_brush(whitesmoke, Colors::WhiteSmoke)
implement_solid_brush(ghostwhite, Colors::GhostWhite)
implement_solid_brush(lightgray, Colors::LightGray)
implement_solid_brush(silver, Colors::Silver)
implement_solid_brush(gray, Colors::Gray)
implement_solid_brush(darkgray, Colors::DarkGray)
implement_solid_brush(dimgray, Colors::DimGray)
implement_solid_brush(orange, Colors::Orange)
implement_solid_brush(khaki, Colors::Khaki)
implement_solid_brush(red, Colors::Red)
implement_solid_brush(yellow, Colors::Yellow)
implement_solid_brush(crimson, Colors::Crimson)
implement_solid_brush(firebrick, Colors::Firebrick)
implement_solid_brush(chocolate, Colors::Chocolate)
implement_solid_brush(greenyellow, Colors::GreenYellow)
implement_solid_brush(forest, Colors::ForestGreen)
implement_solid_brush(cyan, Colors::Cyan)
implement_solid_brush(darkcyan, Colors::DarkCyan)
implement_solid_brush(cadetblue, Colors::CadetBlue)
implement_solid_brush(teal, Colors::Teal)
