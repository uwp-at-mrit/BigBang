#include "gradient.hpp"

using namespace Windows::UI;

using namespace Microsoft::Graphics::Canvas;
using namespace Microsoft::Graphics::Canvas::Brushes;

using namespace Windows::Foundation::Numerics;

static CanvasDevice^ shared_ds = CanvasDevice::GetSharedDevice();

Platform::Array<CanvasGradientStop>^ make_gradient_stops(Color colors[], int total) {
    Platform::Array<CanvasGradientStop>^ stopa = nullptr;
    CanvasGradientStop* stops = new CanvasGradientStop[total];
    auto flstep = 1.0F / float(total - 1);

    for (int i = 0; i < total; i++) {
        stops[i] = CanvasGradientStop{ float(i) * flstep, colors[i] };
    }

    stopa = ref new Platform::Array<CanvasGradientStop>(stops, total);
    delete[] stops;

    return stopa;
}

CanvasLinearGradientBrush^ make_linear_gradient_brush(float sx, float sy, float ex, float ey
    , Platform::Array<CanvasGradientStop>^ stops
    , CanvasEdgeBehavior edge, CanvasAlphaMode alpha) {
    auto brush = ref new CanvasLinearGradientBrush(shared_ds, stops, edge, alpha);

    brush->StartPoint = float2(sx, sy);
    brush->EndPoint = float2(ex, ey);

    return brush;
}
