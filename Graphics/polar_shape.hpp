#pragma once

#include "geometry.hpp"

Microsoft::Graphics::Canvas::Geometry::CanvasGeometry^ polar_axis(float radius, double degrees);
Microsoft::Graphics::Canvas::Geometry::CanvasGeometry^ polar_pole(float radius, double degrees, float pt_radius);

Microsoft::Graphics::Canvas::Geometry::CanvasGeometry^ polar_triangle(float radius, double degrees);
Microsoft::Graphics::Canvas::Geometry::CanvasGeometry^ polar_masked_triangle(float raidus, double degrees, double height_ratio);

Microsoft::Graphics::Canvas::Geometry::CanvasGeometry^ polar_sandglass(float radius, double degrees);
Microsoft::Graphics::Canvas::Geometry::CanvasGeometry^ polar_masked_sandglass(float raidus, double degrees, double height_ratio);

Microsoft::Graphics::Canvas::Geometry::CanvasGeometry^ polar_rectangle(float radius, double degrees);
