#pragma once

Microsoft::Graphics::Canvas::Geometry::CanvasGeometry^ geometry_rotate(
    Microsoft::Graphics::Canvas::Geometry::CanvasGeometry^ geometry, double degrees);

Microsoft::Graphics::Canvas::Geometry::CanvasGeometry^ geometry_rotate(
    Microsoft::Graphics::Canvas::Geometry::CanvasGeometry^ geometry, double degrees, float cx, float cy);

Microsoft::Graphics::Canvas::Geometry::CanvasGeometry^ geometry_linewidth(
    Microsoft::Graphics::Canvas::Geometry::CanvasGeometry^ geometry, float thickness);

Microsoft::Graphics::Canvas::Geometry::CanvasGeometry^ geometry_substract(
    Microsoft::Graphics::Canvas::Geometry::CanvasGeometry^ g1,
    Microsoft::Graphics::Canvas::Geometry::CanvasGeometry^ g2);

Microsoft::Graphics::Canvas::Geometry::CanvasGeometry^ geometry_intersect(
    Microsoft::Graphics::Canvas::Geometry::CanvasGeometry^ g1,
    Microsoft::Graphics::Canvas::Geometry::CanvasGeometry^ g2);

Microsoft::Graphics::Canvas::Geometry::CanvasGeometry^ geometry_union(
    Microsoft::Graphics::Canvas::Geometry::CanvasGeometry^ g1,
    Microsoft::Graphics::Canvas::Geometry::CanvasGeometry^ g2);

Microsoft::Graphics::Canvas::Geometry::CanvasGeometry^ geometry_xor(
    Microsoft::Graphics::Canvas::Geometry::CanvasGeometry^ g1,
    Microsoft::Graphics::Canvas::Geometry::CanvasGeometry^ g2);

/*************************************************************************************************/
Microsoft::Graphics::Canvas::Geometry::CanvasGeometry^ blank();
Microsoft::Graphics::Canvas::Geometry::CanvasGeometry^ vline(float x, float y, float length, float thickness);
Microsoft::Graphics::Canvas::Geometry::CanvasGeometry^ hline(float x, float y, float length, float thickness);
Microsoft::Graphics::Canvas::Geometry::CanvasGeometry^ circle(float cx, float cy, float radius);
Microsoft::Graphics::Canvas::Geometry::CanvasGeometry^ rectangle(float x, float y, float width, float height);

Microsoft::Graphics::Canvas::Geometry::CanvasGeometry^ rounded_rectangle(
    float x, float y, float width, float height, float radiusX, float radiusY = 0.0F);

Microsoft::Graphics::Canvas::Geometry::CanvasGeometry^ rotate_rectangle(
    float x, float y, float width, float height, double degrees);

Microsoft::Graphics::Canvas::Geometry::CanvasGeometry^ rotate_rectangle(
    float x, float y, float width, float height, double degrees, float centerX, float centerY);

/*************************************************************************************************/
Microsoft::Graphics::Canvas::Geometry::CanvasGeometry^ cylinder_surface(
    float x, float y, float radiusX, float radiusY, float height);

Microsoft::Graphics::Canvas::Geometry::CanvasGeometry^ pyramid_surface(
    float x, float y, float radiusT, float radiusB, float radiusY, float height);
