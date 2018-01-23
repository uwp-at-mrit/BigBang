#include "geometry.hpp"
#include "transformation.hpp"

using namespace Microsoft::Graphics::Canvas;
using namespace Microsoft::Graphics::Canvas::Geometry;

using namespace Windows::Foundation;
using namespace Windows::Foundation::Numerics;

CanvasGeometry^ geometry_rotate(CanvasGeometry^ g, double d) {
    Rect region = g->ComputeBounds();

    return geometry_rotate(g, d,
        region.X + region.Width * 0.5F,
        region.Y + region.Height * 0.5F);
}

CanvasGeometry^ geometry_rotate(CanvasGeometry^ g, double degrees, float cx, float cy) {
    return g->Transform(make_rotation_matrix(degrees, cx, cy));
}

CanvasGeometry^ geometry_stroke(CanvasGeometry^ g, float thickness, CanvasStrokeStyle^ style) {
    return (style == nullptr) ? g->Stroke(thickness) : g->Stroke(thickness, style);
}

CanvasGeometry^ geometry_subtract(CanvasGeometry^ g1, CanvasGeometry^ g2, float tx, float ty) {
    return g1->CombineWith(g2, make_translation_matrix(tx, ty), CanvasGeometryCombine::Exclude);
}

CanvasGeometry^ geometry_intersect(CanvasGeometry^ g1, CanvasGeometry^ g2, float tx, float ty) {
    return g1->CombineWith(g2, make_translation_matrix(tx, ty), CanvasGeometryCombine::Intersect);
}

CanvasGeometry^ geometry_union(CanvasGeometry^ g1, CanvasGeometry^ g2, float tx, float ty) {
    return g1->CombineWith(g2, make_translation_matrix(tx, ty), CanvasGeometryCombine::Union);
}

CanvasGeometry^ geometry_xor(CanvasGeometry^ g1, CanvasGeometry^ g2, float tx, float ty) {
    return g1->CombineWith(g2, make_translation_matrix(tx, ty), CanvasGeometryCombine::Xor);
}

CanvasGeometry^ geometry_subtract(CanvasGeometry^ g1, CanvasGeometry^ g2, float3x2 t) {
    return g1->CombineWith(g2, t, CanvasGeometryCombine::Exclude);
}

CanvasGeometry^ geometry_intersect(CanvasGeometry^ g1, CanvasGeometry^ g2, float3x2 t) {
    return g1->CombineWith(g2, t, CanvasGeometryCombine::Intersect);
}

CanvasGeometry^ geometry_union(CanvasGeometry^ g1, CanvasGeometry^ g2, float3x2 t) {
    return g1->CombineWith(g2, t, CanvasGeometryCombine::Union);
}

CanvasGeometry^ geometry_xor(CanvasGeometry^ g1, CanvasGeometry^ g2, float3x2 t) {
    return g1->CombineWith(g2, t, CanvasGeometryCombine::Xor);
}

/*************************************************************************************************/
CanvasCachedGeometry^ geometry_freeze(CanvasGeometry^ geometry) {
    return CanvasCachedGeometry::CreateFill(geometry);
}

CanvasCachedGeometry^ geometry_draft(CanvasGeometry^ geometry, float linewidth) {
    return CanvasCachedGeometry::CreateStroke(geometry, linewidth);
}
