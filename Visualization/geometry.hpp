#pragma once

Microsoft::Graphics::Canvas::Geometry::CanvasGeometry^ make_cylinder_surface(
    Microsoft::Graphics::Canvas::CanvasDrawingSession^ ds,
    float x, float y, float radiusX, float radiusY, float height
);

Microsoft::Graphics::Canvas::Geometry::CanvasGeometry^ make_pyramid_surface(
    Microsoft::Graphics::Canvas::CanvasDrawingSession^ ds,
    float x, float y, float radiusT, float radiusB, float radiusY, float height 
);
