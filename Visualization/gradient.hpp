#pragma once

Microsoft::Graphics::Canvas::Brushes::CanvasLinearGradientBrush^ make_linear_gradient_brush(
    Microsoft::Graphics::Canvas::ICanvasResourceCreator^ master,
    float start_x, float start_y, float end_x, float end_y,
    Microsoft::Graphics::Canvas::Brushes::CanvasGradientStop* stops, int total,
    Microsoft::Graphics::Canvas::CanvasEdgeBehavior shade = Microsoft::Graphics::Canvas::CanvasEdgeBehavior::Mirror,
    Microsoft::Graphics::Canvas::CanvasAlphaMode alpha = Microsoft::Graphics::Canvas::CanvasAlphaMode::Premultiplied
);

Microsoft::Graphics::Canvas::Brushes::CanvasLinearGradientBrush^ make_linear_gradient_brush(
    Microsoft::Graphics::Canvas::ICanvasResourceCreator^ master,
    float start_x, float start_y, float end_x, float end_y, Windows::UI::Color colors[], int total,
    Microsoft::Graphics::Canvas::CanvasEdgeBehavior shade = Microsoft::Graphics::Canvas::CanvasEdgeBehavior::Mirror,
    Microsoft::Graphics::Canvas::CanvasAlphaMode alpha = Microsoft::Graphics::Canvas::CanvasAlphaMode::Premultiplied
);
