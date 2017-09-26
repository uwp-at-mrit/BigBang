#define _USE_MATH_DEFINES
#include <cmath>

#include "path.hpp"

using namespace Microsoft::Graphics::Canvas;
using namespace Microsoft::Graphics::Canvas::Geometry;

using namespace Windows::Foundation::Numerics;

CanvasGeometry^ make_cylinder_surface(CanvasDrawingSession^ ds, float x, float y, float w, float h, float ry) {
    auto surface = ref new CanvasPathBuilder(ds);
    auto rx = w / 2.0F;
    
    surface->BeginFigure(x, y);
    surface->AddArc(float2(x + rx, y + ry), rx, ry, float(M_PI), -float(M_PI));
    surface->AddLine(x + w, y + h);
    surface->AddArc(float2(x + rx, y + ry + h), rx, ry, 0.0F, float(M_PI));
    surface->EndFigure(CanvasFigureLoop::Closed);

    return CanvasGeometry::CreatePath(surface);
}
