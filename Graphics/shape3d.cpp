#define _USE_MATH_DEFINES
#include <WindowsNumerics.h>

#include "shape3d.hpp"

using namespace Windows::Foundation::Numerics;

using namespace Microsoft::Graphics::Canvas;
using namespace Microsoft::Graphics::Canvas::Geometry;


CanvasGeometry^ cylinder_tb_surface(float x, float y, float rx, float ry, float height) {
    auto surface = ref new CanvasPathBuilder(CanvasDevice::GetSharedDevice());
    float cx = x + rx;
    float cy = y + ry;
    
    surface->BeginFigure(x, y);
    surface->AddArc(float2(cx, cy), rx, ry, float(M_PI), -float(M_PI));
    surface->AddLine(cx + rx, y + height);
    surface->AddArc(float2(cx, cy + height), rx, ry, 0.0F, float(M_PI));
    surface->EndFigure(CanvasFigureLoop::Closed);

    return CanvasGeometry::CreatePath(surface);
}

CanvasGeometry^ cylinder_rl_surface(float x, float y, float rx, float ry, float width) {
    auto surface = ref new CanvasPathBuilder(CanvasDevice::GetSharedDevice());
    float cx = x + rx;
    float cy = y + ry;

    surface->BeginFigure(cx, y);
    surface->AddArc(float2(cx, cy), rx, ry, -float(M_PI_2), -float(M_PI));
    surface->AddLine(cx + width, cy + ry);
    surface->AddArc(float2(cx + width, cy), rx, ry, float(M_PI_2), float(M_PI));
    surface->EndFigure(CanvasFigureLoop::Closed);

    return CanvasGeometry::CreatePath(surface);
}

CanvasGeometry^ cylinder_lr_surface(float x, float y, float rx, float ry, float width) {
    auto surface = ref new CanvasPathBuilder(CanvasDevice::GetSharedDevice());
    float cx = x + rx;
    float cy = y + ry;

    surface->BeginFigure(cx, y);
    surface->AddArc(float2(cx, cy), rx, ry, -float(M_PI_2), float(M_PI));
    surface->AddLine(cx + width, cy + ry);
    surface->AddArc(float2(cx + width, cy), rx, ry, float(M_PI_2), -float(M_PI));
    surface->EndFigure(CanvasFigureLoop::Closed);

    return CanvasGeometry::CreatePath(surface);
}

CanvasGeometry^ pyramid_surface(float x, float y, float rt, float rb, float ry, float height) {
    auto surface = ref new CanvasPathBuilder(CanvasDevice::GetSharedDevice());
    float cx = x + rt;
    float cy = y + ry;

    surface->BeginFigure(x, y);
    surface->AddArc(float2(cx, cy), rt, ry, float(M_PI), -float(M_PI));
    surface->AddLine(cx + rb, y + height);
    surface->AddArc(float2(cx, cy + height), rb, ry * rb / rt, 0.0F, float(M_PI));
    surface->EndFigure(CanvasFigureLoop::Closed);

    return CanvasGeometry::CreatePath(surface);
}

CanvasGeometry^ cylinder_tb_surface(float rx, float ry, float height) {
    return cylinder_tb_surface(0.0F, 0.0F, rx, ry, height);
}

CanvasGeometry^ cylinder_rl_surface(float rx, float ry, float width) {
    return cylinder_rl_surface(0.0F, 0.0F, rx, ry, width);
}

CanvasGeometry^ cylinder_lr_surface(float rx, float ry, float width) {
    return cylinder_lr_surface(0.0F, 0.0F, rx, ry, width);
}

CanvasGeometry^ pyramid_surface(float rt, float rb, float ry, float height) {
    return pyramid_surface(0.0F, 0.0F, rt, rb, ry, height);
}
