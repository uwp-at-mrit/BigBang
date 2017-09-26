#pragma once

Microsoft::Graphics::Canvas::Geometry::CanvasGeometry^ make_cylinder_surface(
    Microsoft::Graphics::Canvas::CanvasDrawingSession^ ds,
    float x, float y, float radiusX, float radiusY, float height
);
