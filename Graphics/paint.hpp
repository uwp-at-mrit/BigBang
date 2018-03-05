#pragma once

typedef Platform::Array<Microsoft::Graphics::Canvas::Brushes::CanvasGradientStop> GradientStops;

#define MAKE_GRADIENT_STOPS(colors) make_gradient_stops(colors, sizeof(colors) / sizeof(Windows::UI::Color))

GradientStops^ make_gradient_stops(Windows::UI::Color colors[], int total);
void brush_translate(Microsoft::Graphics::Canvas::Brushes::ICanvasBrush^ brush, float x, float y);

Microsoft::Graphics::Canvas::Brushes::CanvasSolidColorBrush^ make_solid_brush(Windows::UI::Color& color);

Microsoft::Graphics::Canvas::Brushes::CanvasLinearGradientBrush^ make_linear_gradient_brush(
    float hextent, float vextent, GradientStops^ stops,
    Microsoft::Graphics::Canvas::CanvasEdgeBehavior shade = Microsoft::Graphics::Canvas::CanvasEdgeBehavior::Mirror,
    Microsoft::Graphics::Canvas::CanvasAlphaMode alpha = Microsoft::Graphics::Canvas::CanvasAlphaMode::Premultiplied);

Microsoft::Graphics::Canvas::Brushes::CanvasLinearGradientBrush^ make_linear_gradient_brush(
    float start_x, float start_y, float end_x, float end_y, GradientStops^ stops,
    Microsoft::Graphics::Canvas::CanvasEdgeBehavior shade = Microsoft::Graphics::Canvas::CanvasEdgeBehavior::Mirror,
    Microsoft::Graphics::Canvas::CanvasAlphaMode alpha = Microsoft::Graphics::Canvas::CanvasAlphaMode::Premultiplied);

Microsoft::Graphics::Canvas::Geometry::CanvasStrokeStyle^ make_dash_stroke(Platform::Array<float>^ dashes, float offset = 0.0F);
Microsoft::Graphics::Canvas::Geometry::CanvasStrokeStyle^ make_dash_stroke(
    Microsoft::Graphics::Canvas::Geometry::CanvasDashStyle style,
    float offset = 0.0F);

/*************************************************************************************************/
Microsoft::Graphics::Canvas::Brushes::CanvasSolidColorBrush^ system_accentdark1_brush();

Microsoft::Graphics::Canvas::Brushes::CanvasSolidColorBrush^ system_background_brush();
Microsoft::Graphics::Canvas::Brushes::CanvasSolidColorBrush^ system_foreground_brush();
Microsoft::Graphics::Canvas::Brushes::CanvasSolidColorBrush^ system_highlight_brush();
Microsoft::Graphics::Canvas::Brushes::CanvasSolidColorBrush^ system_graytext_brush();
