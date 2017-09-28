#pragma once

Microsoft::Graphics::Canvas::Geometry::CanvasGeometry^ geometry_rotate(
    Microsoft::Graphics::Canvas::Geometry::CanvasGeometry^ geometry, double degrees);

Microsoft::Graphics::Canvas::Geometry::CanvasGeometry^ geometry_rotate(
    Microsoft::Graphics::Canvas::Geometry::CanvasGeometry^ geometry, double degrees, float cx, float cy);

Microsoft::Graphics::Canvas::Geometry::CanvasGeometry^ geometry_combine(
    Microsoft::Graphics::Canvas::Geometry::CanvasGeometry^ geometry1,
    Microsoft::Graphics::Canvas::Geometry::CanvasGeometry^ geometry2,
    Microsoft::Graphics::Canvas::Geometry::CanvasGeometryCombine method);

/*************************************************************************************************/
Microsoft::Graphics::Canvas::Geometry::CanvasGeometry^ rectangle(
    float x, float y, float radiusX, float radiusY);

Microsoft::Graphics::Canvas::Geometry::CanvasGeometry^ cylinder_surface(
    float x, float y, float radiusX, float radiusY, float height);

Microsoft::Graphics::Canvas::Geometry::CanvasGeometry^ pyramid_surface(
    float x, float y, float radiusT, float radiusB, float radiusY, float height);

Microsoft::Graphics::Canvas::Geometry::CanvasGeometry^ rotate_rectangle(
    float x, float y, float width, float height, double degrees);

Microsoft::Graphics::Canvas::Geometry::CanvasGeometry^ rotate_rectangle(
    float x, float y, float width, float height, double degrees, float centerX, float centerY);
