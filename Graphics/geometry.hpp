#pragma once

#define GEOMETRY_UNION(gs) geometry_union(gs, sizeof(gs) / sizeof(Microsoft::Graphics::Canvas::Geometry::CanvasGeometry^))

Microsoft::Graphics::Canvas::Geometry::CanvasGeometry^ geometry_rotate(
    Microsoft::Graphics::Canvas::Geometry::CanvasGeometry^ geometry, double degrees);

Microsoft::Graphics::Canvas::Geometry::CanvasGeometry^ geometry_rotate(
    Microsoft::Graphics::Canvas::Geometry::CanvasGeometry^ geometry, double degrees, float cx, float cy);

Microsoft::Graphics::Canvas::Geometry::CanvasGeometry^ geometry_stroke(
    Microsoft::Graphics::Canvas::Geometry::CanvasGeometry^ geometry, float thickness,
    Microsoft::Graphics::Canvas::Geometry::CanvasStrokeStyle^ style = nullptr);

Microsoft::Graphics::Canvas::Geometry::CanvasGeometry^ geometry_subtract(
    Microsoft::Graphics::Canvas::Geometry::CanvasGeometry^ g1,
    Microsoft::Graphics::Canvas::Geometry::CanvasGeometry^ g2,
    float tx = 0.0F, float ty = 0.0F);

Microsoft::Graphics::Canvas::Geometry::CanvasGeometry^ geometry_intersect(
    Microsoft::Graphics::Canvas::Geometry::CanvasGeometry^ g1,
    Microsoft::Graphics::Canvas::Geometry::CanvasGeometry^ g2,
    float tx = 0.0F, float ty = 0.0F);

Microsoft::Graphics::Canvas::Geometry::CanvasGeometry^ geometry_union(
    Microsoft::Graphics::Canvas::Geometry::CanvasGeometry^ g1,
    Microsoft::Graphics::Canvas::Geometry::CanvasGeometry^ g2,
    float tx = 0.0F, float ty = 0.0F);

Microsoft::Graphics::Canvas::Geometry::CanvasGeometry^ geometry_union(
	Microsoft::Graphics::Canvas::Geometry::CanvasGeometry^ gs[],
	size_t count);

Microsoft::Graphics::Canvas::Geometry::CanvasGeometry^ geometry_xor(
    Microsoft::Graphics::Canvas::Geometry::CanvasGeometry^ g1,
    Microsoft::Graphics::Canvas::Geometry::CanvasGeometry^ g2,
    float tx = 0.0F, float ty = 0.0F);

Microsoft::Graphics::Canvas::Geometry::CanvasGeometry^ geometry_subtract(
    Microsoft::Graphics::Canvas::Geometry::CanvasGeometry^ g1,
    Microsoft::Graphics::Canvas::Geometry::CanvasGeometry^ g2,
    Windows::Foundation::Numerics::float3x2 transform);

Microsoft::Graphics::Canvas::Geometry::CanvasGeometry^ geometry_intersect(
    Microsoft::Graphics::Canvas::Geometry::CanvasGeometry^ g1,
    Microsoft::Graphics::Canvas::Geometry::CanvasGeometry^ g2,
    Windows::Foundation::Numerics::float3x2 transform);

Microsoft::Graphics::Canvas::Geometry::CanvasGeometry^ geometry_union(
    Microsoft::Graphics::Canvas::Geometry::CanvasGeometry^ g1,
    Microsoft::Graphics::Canvas::Geometry::CanvasGeometry^ g2,
    Windows::Foundation::Numerics::float3x2 transform);

Microsoft::Graphics::Canvas::Geometry::CanvasGeometry^ geometry_xor(
    Microsoft::Graphics::Canvas::Geometry::CanvasGeometry^ g1,
    Microsoft::Graphics::Canvas::Geometry::CanvasGeometry^ g2,
    Windows::Foundation::Numerics::float3x2 transform);

/*************************************************************************************************/
Microsoft::Graphics::Canvas::Geometry::CanvasCachedGeometry^ geometry_freeze(
    Microsoft::Graphics::Canvas::Geometry::CanvasGeometry^ geometry);

Microsoft::Graphics::Canvas::Geometry::CanvasCachedGeometry^ geometry_draft(
    Microsoft::Graphics::Canvas::Geometry::CanvasGeometry^ geometry,
    float linewidth = 1.0F);
