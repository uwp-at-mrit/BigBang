#include "gradient.hpp"

using namespace Windows::UI;

using namespace Microsoft::Graphics::Canvas;
using namespace Microsoft::Graphics::Canvas::Brushes;

using namespace Windows::Foundation::Numerics;

CanvasLinearGradientBrush^ make_linear_gradient_brush(ICanvasResourceCreator^ master
    , float sx, float sy, float ex, float ey, CanvasGradientStop* stops, int total
    , CanvasEdgeBehavior edge, CanvasAlphaMode alpha) {
    
    /** NOTE
     * Uses `auto` here to define 'stopa' may cause warning: using uninitialized memory.
     */
    Platform::Array<CanvasGradientStop>^ stopa = ref new Platform::Array<CanvasGradientStop>(stops, total);
    auto brush = ref new CanvasLinearGradientBrush(master, stopa, edge, alpha);

    brush->StartPoint = float2(sx, sy);
    brush->EndPoint = float2(ex, ey);

    return brush;
}

CanvasLinearGradientBrush^ make_linear_gradient_brush(ICanvasResourceCreator^ master
    , float sx, float sy, float ex, float ey, Color colors[], int total
    , CanvasEdgeBehavior edge, CanvasAlphaMode alpha) {
    CanvasGradientStop* stops = new CanvasGradientStop[total];
    auto flstep = 1.0F / float(total - 1);
    
    for (int i = 0; i < total; i++) {
        stops[i] = CanvasGradientStop{float(i) * flstep, colors[i]};
    }

    auto brush = make_linear_gradient_brush(master, sx, sy, ex, ey, stops, total, edge, alpha);
    delete [] stops;

    return brush;
}
