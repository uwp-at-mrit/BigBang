#include "paint.hpp"
#include "system.hpp"
#include "colorspace.hpp"
#include "transformation.hpp"

using namespace WarGrey::SCADA;

using namespace Windows::UI;
using namespace Windows::UI::ViewManagement;

using namespace Windows::Foundation::Numerics;

using namespace Microsoft::Graphics::Canvas;
using namespace Microsoft::Graphics::Canvas::Brushes;
using namespace Microsoft::Graphics::Canvas::Geometry;

static inline GradientStops^ make_and_clean_gradient_stops(CanvasGradientStop* stops, size_t total) {
	GradientStops^ stopa = ref new Platform::Array<CanvasGradientStop>(stops, (unsigned int)(total));
	
	delete[] stops;

	return stopa;
}

GradientStops^ WarGrey::SCADA::make_gradient_stops(CanvasSolidColorBrush^ brushes[], size_t total) {
	CanvasGradientStop* stops = new CanvasGradientStop[total];
	auto flstep = 1.0F / float(total - 1);

	for (int i = 0; i < total; i++) {
		stops[i].Position = float(i) * flstep;
		stops[i].Color = brushes[i]->Color;
	}

	return make_and_clean_gradient_stops(stops, total);
}

GradientStops^ WarGrey::SCADA::make_gradient_stops(CanvasSolidColorBrush^ brushes[], float positions[], size_t total) {
	CanvasGradientStop* stops = new CanvasGradientStop[total];
	
	for (int i = 0; i < total; i++) {
		stops[i].Position = positions[i];
		stops[i].Color = brushes[i]->Color;
	}

	return make_and_clean_gradient_stops(stops, total);
}

GradientStops^ WarGrey::SCADA::make_gradient_stops(Color colors[], size_t total) {
	CanvasGradientStop* stops = new CanvasGradientStop[total];
	auto flstep = 1.0F / float(total - 1);

	for (int i = 0; i < total; i++) {
		stops[i].Position = float(i) * flstep;
		stops[i].Color = colors[i];
	}

	return make_and_clean_gradient_stops(stops, total);
}

GradientStops^ WarGrey::SCADA::make_gradient_stops(Color colors[], float positions[], size_t total) {
	CanvasGradientStop* stops = new CanvasGradientStop[total];
	
	for (int i = 0; i < total; i++) {
		stops[i].Position = positions[i];
		stops[i].Color = colors[i];
	}

	return make_and_clean_gradient_stops(stops, total);
}

GradientStops^ WarGrey::SCADA::make_gradient_stops(unsigned int hexes[], size_t total) {
	CanvasGradientStop* stops = new CanvasGradientStop[total];
	auto flstep = 1.0F / float(total - 1);

	for (int i = 0; i < total; i++) {
		stops[i].Position = float(i) * flstep;
		stops[i].Color = rgba(hexes[i]);
	}

	return make_and_clean_gradient_stops(stops, total);
}

GradientStops^ WarGrey::SCADA::make_gradient_stops(unsigned int hexes[], float positions[], size_t total) {
	CanvasGradientStop* stops = new CanvasGradientStop[total];
	
	for (int i = 0; i < total; i++) {
		stops[i].Position = positions[i];
		stops[i].Color = rgba(hexes[i]);
	}

	return make_and_clean_gradient_stops(stops, total);
}

Color WarGrey::SCADA::gradient_discrete_color(GradientStops^ stops, float percentage) {
	Color* c = nullptr;

	for (int idx = stops->Length - 1; idx > 0; idx--) {
		if (stops[idx].Position <= percentage) {
			c = &(stops[idx].Color);
			break;
		}
	}

	return ((c == nullptr) ? stops[0].Color : (*c));
}

void WarGrey::SCADA::brush_translate(ICanvasBrush^ brush, float x, float y) {
    brush->Transform = make_translation_matrix(x, y);
}

CanvasSolidColorBrush^ WarGrey::SCADA::make_solid_brush(Color& color) {
    return ref new CanvasSolidColorBrush(CanvasDevice::GetSharedDevice(), color);
}

CanvasSolidColorBrush^ WarGrey::SCADA::make_solid_brush(unsigned int hex, double alpha) {
	return ref new CanvasSolidColorBrush(CanvasDevice::GetSharedDevice(), rgba(hex, alpha));
}

CanvasLinearGradientBrush^ WarGrey::SCADA::make_linear_gradient_brush(float sx, float sy, float ex, float ey
    , GradientStops^ stops, CanvasEdgeBehavior edge, CanvasAlphaMode alpha) {
    auto brush = ref new CanvasLinearGradientBrush(CanvasDevice::GetSharedDevice(), stops, edge, alpha);

    brush->StartPoint = float2(sx, sy);
    brush->EndPoint = float2(ex, ey);

    return brush;
}

CanvasLinearGradientBrush^ WarGrey::SCADA::make_linear_gradient_brush(float hextent, float vextent, Color& src
	, float light_scale, float dark_scale, CanvasEdgeBehavior edge, CanvasAlphaMode alpha) {
	Color colors[2];

	colors[0] = scale_color(src, 1.618);
	colors[1] = scale_color(src, 0.618);

	return make_linear_gradient_brush(hextent, vextent, make_gradient_stops(colors), edge, alpha); // Don't mind, it Visual Studio's fault
}

CanvasLinearGradientBrush^ WarGrey::SCADA::make_linear_gradient_brush(float hextent, float vextent, GradientStops^ stops
    , CanvasEdgeBehavior edge, CanvasAlphaMode alpha) {
    return make_linear_gradient_brush(0.0F, 0.0F, hextent, vextent, stops, edge, alpha);
}

CanvasStrokeStyle^ WarGrey::SCADA::make_dash_stroke(Platform::Array<float>^ dashes, float offset) {
    auto dash = ref new CanvasStrokeStyle();

    dash->DashOffset = offset;
    dash->CustomDashStyle = dashes;

    return dash;
}

CanvasStrokeStyle^ WarGrey::SCADA::make_dash_stroke(CanvasDashStyle style, float offset) {
    auto dash = ref new CanvasStrokeStyle();
    dash->DashOffset = offset;
    dash->DashStyle = style;

    return dash;
}

CanvasStrokeStyle^ WarGrey::SCADA::make_roundcap_stroke_style(bool shared) {
	static auto shared_style = ref new CanvasStrokeStyle();
	auto s = (shared ? shared_style : ref new CanvasStrokeStyle());

	s->StartCap = CanvasCapStyle::Round;
	s->EndCap = CanvasCapStyle::Round;

	return s;
}
