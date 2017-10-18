#pragma once

#define MAKE_GRADIENT_STOPS(colors) make_gradient_stops(colors, sizeof(colors) / sizeof(Windows::UI::Color))

Platform::Array<Microsoft::Graphics::Canvas::Brushes::CanvasGradientStop>^ make_gradient_stops(
    Windows::UI::Color colors[], int total);

Microsoft::Graphics::Canvas::Brushes::CanvasLinearGradientBrush^ make_linear_gradient_brush(
    float start_x, float start_y, float end_x, float end_y,
    Platform::Array<Microsoft::Graphics::Canvas::Brushes::CanvasGradientStop>^ stops,
    Microsoft::Graphics::Canvas::CanvasEdgeBehavior shade = Microsoft::Graphics::Canvas::CanvasEdgeBehavior::Mirror,
    Microsoft::Graphics::Canvas::CanvasAlphaMode alpha = Microsoft::Graphics::Canvas::CanvasAlphaMode::Premultiplied);
