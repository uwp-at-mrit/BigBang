#include "geometry.hpp"
#include "shape.hpp"
#include "transformation.hpp"

using namespace WarGrey::SCADA;

using namespace Microsoft::Graphics::Canvas;
using namespace Microsoft::Graphics::Canvas::Geometry;

using namespace Windows::Foundation;
using namespace Windows::Foundation::Numerics;

CanvasGeometry^ WarGrey::SCADA::geometry_rotate(CanvasGeometry^ g, double d) {
    Rect region = g->ComputeBounds();

    return geometry_rotate(g, d,
        region.X + region.Width * 0.5F,
        region.Y + region.Height * 0.5F);
}

CanvasGeometry^ WarGrey::SCADA::geometry_rotate(CanvasGeometry^ g, double degrees, float cx, float cy) {
    return g->Transform(make_rotation_matrix(degrees, cx, cy));
}

CanvasGeometry^ WarGrey::SCADA::geometry_stroke(CanvasGeometry^ g, float thickness, CanvasStrokeStyle^ style) {
    return (style == nullptr) ? g->Stroke(thickness) : g->Stroke(thickness, style);
}

CanvasGeometry^ WarGrey::SCADA::geometry_translate(CanvasGeometry^ g, float tx, float ty) {
	return g->Transform(make_translation_matrix(tx, ty));
}

CanvasGeometry^ WarGrey::SCADA::geometry_scale(CanvasGeometry^ g, float sx, float sy) {
	return g->Transform(make_scale_matrix(sx, sy));
}

CanvasGeometry^ WarGrey::SCADA::geometry_subtract(CanvasGeometry^ g1, CanvasGeometry^ g2, float tx, float ty) {
    return g1->CombineWith(g2, make_translation_matrix(tx, ty), CanvasGeometryCombine::Exclude);
}

CanvasGeometry^ WarGrey::SCADA::geometry_intersect(CanvasGeometry^ g1, CanvasGeometry^ g2, float tx, float ty) {
    return g1->CombineWith(g2, make_translation_matrix(tx, ty), CanvasGeometryCombine::Intersect);
}

CanvasGeometry^ WarGrey::SCADA::geometry_xor(CanvasGeometry^ g1, CanvasGeometry^ g2, float tx, float ty) {
	return g1->CombineWith(g2, make_translation_matrix(tx, ty), CanvasGeometryCombine::Xor);
}

CanvasGeometry^ WarGrey::SCADA::geometry_union(CanvasGeometry^ g1, CanvasGeometry^ g2, float tx, float ty) {
    return g1->CombineWith(g2, make_translation_matrix(tx, ty), CanvasGeometryCombine::Union);
}

CanvasGeometry^ WarGrey::SCADA::geometry_subtract(CanvasGeometry^ g0, float tx1, float ty1, CanvasGeometry^ g2, float tx2, float ty2) {
	return geometry_subtract(g0->Transform(make_translation_matrix(tx1, ty1)), g2, tx2, ty2);
}

CanvasGeometry^ WarGrey::SCADA::geometry_intersect(CanvasGeometry^ g0, float tx1, float ty1, CanvasGeometry^ g2, float tx2, float ty2) {
	return geometry_intersect(g0->Transform(make_translation_matrix(tx1, ty1)), g2, tx2, ty2);
}

CanvasGeometry^ WarGrey::SCADA::geometry_xor(CanvasGeometry^ g0, float tx1, float ty1, CanvasGeometry^ g2, float tx2, float ty2) {
	return geometry_xor(g0->Transform(make_translation_matrix(tx1, ty1)), g2, tx2, ty2);
}

CanvasGeometry^ WarGrey::SCADA::geometry_union(CanvasGeometry^ g0, float tx1, float ty1, CanvasGeometry^ g2, float tx2, float ty2) {
	return geometry_union(g0->Transform(make_translation_matrix(tx1, ty1)), g2, tx2, ty2);
}

CanvasGeometry^ WarGrey::SCADA::geometry_subtract(CanvasGeometry^ g, float tx1, float ty1, float tx2, float ty2) {
	return geometry_subtract(g, tx1, ty1, g, tx2, ty2);
}

CanvasGeometry^ WarGrey::SCADA::geometry_intersect(CanvasGeometry^ g, float tx1, float ty1, float tx2, float ty2) {
	return geometry_intersect(g, tx1, ty1, g, tx2, ty2);
}

CanvasGeometry^ WarGrey::SCADA::geometry_xor(CanvasGeometry^ g, float tx1, float ty1, float tx2, float ty2) {
	return geometry_xor(g, tx1, ty1, g, tx2, ty2);
}

CanvasGeometry^ WarGrey::SCADA::geometry_union(CanvasGeometry^ g, float tx1, float ty1, float tx2, float ty2) {
	return geometry_union(g, tx1, ty1, g, tx2, ty2);
}

CanvasGeometry^ WarGrey::SCADA::geometry_union(CanvasGeometry^ gs[], size_t count) {
	CanvasGeometry^ g = ((count > 0) ? gs[0] : blank());
	
	for (size_t i = 1; i < count; i++) {
		g = geometry_union(g, gs[i]);
	}

	return g;
}

/*************************************************************************************************/
CanvasCachedGeometry^ WarGrey::SCADA::geometry_freeze(CanvasGeometry^ geometry) {
    return CanvasCachedGeometry::CreateFill(geometry);
}

CanvasCachedGeometry^ WarGrey::SCADA::geometry_draft(CanvasGeometry^ geometry, float linewidth, CanvasStrokeStyle^ style) {
    return (style == nullptr)
		? CanvasCachedGeometry::CreateStroke(geometry, linewidth)
		: CanvasCachedGeometry::CreateStroke(geometry, linewidth, style);
}
